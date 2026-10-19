// Render harness for the LOW middle section, on jsbeeb (BBC B, 8271 DFS 1.2).
//
// Boots the release disc (build/beeb-niccc.dsd) through the loader and intro,
// then, on the second arrival at &1100 (the intro chaining to /LOW), overwrites
// the loaded LOW with the candidate build and lets it run to the end of the stream.
// The disc layout stays that of the release build, so the candidate may grow
// without the stream moving off track 30.
//
// For every frame it records, at swap_frame_buffers:
//   - SHA-1 of the 6,400-byte buffer just drawn plus the 16 ULA palette registers
//   - render cycles: cycles executed from parse_frame entry to its return,
//     excluding interrupt handlers (MOS, NMI at &0D00, music in SWRAM, irq_handler)
//     and the 7-cycle interrupt entry sequences
//
// Usage: node tools/harness/run.mjs [--ref ref.json] [--out out.json] [--dump dir] [--profile prof.txt]
//        [--spans spans.json [--spans-frames N]]  (_FAST_POLY builds: span buffer per polygon)
//        [--disc image.dsd] [--no-inject]  (run the LOW on the disc instead: checks a full disc build)
// Environment: JSBEEB (default: newest jsbeeb in the npx cache, as the MCP uses),
//              BEEBASM (default ../beebasm/beebasm.exe)

import { execFileSync } from "child_process";
import { createHash } from "crypto";
import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync } from "fs";
import path from "path";
import { pathToFileURL } from "url";

const repo = path.resolve(path.dirname(new URL(import.meta.url).pathname.replace(/^\/([A-Za-z]:)/, "$1")), "../..");
const args = process.argv.slice(2);
const opt = (name, def) => {
    const i = args.indexOf(name);
    return i >= 0 ? args[i + 1] : def;
};

// The jsbeeb the MCP server runs: `npx jsbeeb-mcp` keeps it in the npx cache.
// Take the newest jsbeeb found there unless JSBEEB names a checkout.
function findJsbeeb() {
    if (process.env.JSBEEB) return path.resolve(repo, process.env.JSBEEB);
    const npx = path.join(process.env.LOCALAPPDATA ?? "", "npm-cache", "_npx");
    const found = [];
    for (const d of existsSync(npx) ? readdirSync(npx) : []) {
        const pkg = path.join(npx, d, "node_modules", "jsbeeb", "package.json");
        if (existsSync(pkg)) found.push({ dir: path.dirname(pkg), v: JSON.parse(readFileSync(pkg)).version });
    }
    const key = (v) => v.split(".").map((n) => n.padStart(6, "0")).join(".");
    found.sort((a, b) => (key(a.v) < key(b.v) ? 1 : -1));
    if (!found.length) throw new Error("no jsbeeb in the npx cache; run the jsbeeb MCP once or set JSBEEB");
    return found[0].dir;
}
const jsbeeb = findJsbeeb();
const beebasm = path.resolve(repo, process.env.BEEBASM ?? "../beebasm/beebasm.exe");
// --variant nula runs the NuLA player (the intro chains to /NULA) and hashes the
// rendered NuLA palette; --src assembles another tree (e.g. a worktree of the
// release, with --disc of the release image, for a baseline).
const variant = opt("--variant", "low");
const VARIANTS = {
    low: { defines: ["_QUALITY=0", "_NULA=0"], exe: "LOW" },
    nula: { defines: ["_QUALITY=1", "_NULA=-1"], exe: "NULA" },
    cpc: { defines: ["_QUALITY=1", "_NULA=-1", "_CPC=-1"], exe: "CPC" }, // the intro's choice without a NuLA
    // the CPC version from the column stream, on its own disc (stnicc-cols.asm)
    cols: { defines: [], exe: "CPCCOL", source: "stnicc-cols.asm", disc: "build/beeb-niccc-cpc.dsd" },
    // all 1800 frames from the version 2 column stream (stnicc-cols2.asm)
    cols2: { defines: [], exe: "CPCCOL2", source: "stnicc-cols2.asm", disc: "build/beeb-niccc-cpc1800.dsd",
        outro: { defines: ["_COLS=-1"], exe: "OUTROC2" } },
};
if (!VARIANTS[variant]) throw new Error(`--variant ${variant}: low, nula, cpc, cols or cols2`);
const disc = path.resolve(repo, opt("--disc", VARIANTS[variant].disc ?? "build/beeb-niccc.dsd"));
const src = path.resolve(repo, opt("--src", "."));
const lowPath = path.join(src, "build", VARIANTS[variant].exe);
const refPath = opt("--ref");
const outPath = opt("--out");
const dumpDir = opt("--dump");
const profilePath = opt("--profile");

// ---------------------------------------------------------------- build + symbols

const parseOnly = args.includes("--parse-only");

function buildLow() {
    // -d dumps every symbol as 'name':valueL, comma separated
    const defines = VARIANTS[variant].defines.flatMap((d) => ["-D", d]);
    if (parseOnly) defines.push("-D", "_TEST_PARSE_ONLY=-1");
    const out = execFileSync(beebasm, [...defines, "-i", VARIANTS[variant].source ?? "stnicc-beeb.asm", "-d", "-v"], {
        cwd: src,
        maxBuffer: 64 << 20,
    }).toString();
    writeFileSync(path.join(src, "build", `${variant}.txt`), out);
    for (const line of out.split("\n")) {
        if (line.includes("WARNING")) console.log("beebasm: " + line.trim());
    }
    const syms = {};
    for (const m of out.matchAll(/'([A-Za-z_0-9]+)':(-?\d+)L/g)) syms[m[1]] = Number(m[2]);
    return syms;
}

const syms = buildLow(); // also SAVEs build/LOW
// track_load_error is local to main's scope, so -d does not export it: take it from the listing.
{
    const lst = readFileSync(path.join(src, "build", `${variant}.txt`), "utf8");
    const m = lst.match(/\.track_load_error\s*\n\s*([0-9A-F]{4})/);
    if (!m) throw new Error("track_load_error not found in listing");
    syms.track_load_error = parseInt(m[1], 16);
}
for (const s of ["main", "parse_frame", "swap_frame_buffers", "irq_handler", "draw_buffer_HI", "vsync_final", "frame_no", "error_flag"]) {
    if (syms[s] === undefined) throw new Error(`symbol ${s} missing from beebasm -d output`);
}
const lowBin = readFileSync(lowPath);

// Return site of "jsr parse_frame" inside irq_handler: the instruction after it.
// Found by scanning the code for JSR parse_frame (20 lo hi).
function findReturnSite() {
    const base = syms.main;
    for (let i = syms.irq_handler - base; i < lowBin.length - 2; i++) {
        if (lowBin[i] === 0x20 && lowBin[i + 1] === (syms.parse_frame & 0xff) && lowBin[i + 2] === syms.parse_frame >> 8)
            return base + i + 3;
    }
    throw new Error("jsr parse_frame not found in irq_handler");
}
const parseReturn = findReturnSite();

// --stream-log f.json: every track read (start, end, drive, track, ring slot)
// and every frame start (cycle, stream pointer page), to work out how close
// each read came to starving the decoder (tools/stream_margin.py).
const streamLogPath = opt("--stream-log");
const loadReturns = new Set();
for (let i = 0; i < lowBin.length - 2; i++) {
    if (lowBin[i] === 0x20 && lowBin[i + 1] === (syms.load_next_track & 0xff) && lowBin[i + 2] === syms.load_next_track >> 8)
        loadReturns.add(syms.main + i + 3);
}
const streamLog = streamLogPath ? { ring: [syms.STREAM_buffer_start >> 8, syms.STREAM_buffer_end >> 8], loads: [], frames: [], locks: [] } : null;
let loadOpen = null;
let lockOn = null;

// ---------------------------------------------------------------- machine

const { MachineSession } = await import(pathToFileURL(path.join(jsbeeb, "src/machine-session.js")).href);
const s = new MachineSession("B-DFS1.2");
await s.initialise();
await s.boot();
const cpu = s._machine.processor;
const mem = (a) => cpu.readmem(a);

// jsbeeb 2.9's 8271 now needs a run of index pulses after spin-up before it reports
// the drive ready. When the stream buffer is full the player idles long enough for
// the 8271's automatic head unload, and its next read (the first on side 2, around
// frame 353) fails at once with &10, drive not ready. Real hardware and older jsbeeb
// play through. Keep the head loaded so the harness can reach the end of the stream;
// this changes disc timing only, never render cycles or pixels.
if (!process.env.HARNESS_KEEP_HEAD_UNLOAD) {
    const fdc = cpu.fdc;
    if (typeof fdc._spinDown !== "function") throw new Error("jsbeeb 8271 has no _spinDown to patch");
    // As if the head unload count were &F, "never unload".
    fdc._spinDown = function () {};
}

const t0 = Date.now();
s.loadDisc(disc);
s.keyDown("ShiftLeft");
s.reset(true);
await s.runFor(2_000_000);
s.keyUp("ShiftLeft");

async function runUntilPc(target, maxCycles) {
    let hit = false;
    const hook = cpu.debugInstruction.add((pc) => {
        if (pc === target) {
            hit = true;
            return true;
        }
    });
    await s.runFor(maxCycles);
    hook.remove();
    if (!hit) throw new Error(`did not reach ${target.toString(16)} in ${maxCycles} cycles`);
}

// First &1100 is INTRO: set the NuLA flags so it chains to /LOW (clear) or /NULA (set).
await runUntilPc(0x1100, 120_000_000);
// &7E bit 7: NuLA; bit 6: the CPC version (C in the intro)
// --press-c: leave the flag clear and choose the CPC version with the intro's
// C key instead (held for a few frames, as its debounce needs), so --no-inject
// checks that the key reaches /CPC. With --with-nula the loader has found NuLA
// and selected it, and C must switch NuLA off.
const pressC = args.includes("--press-c");
const withNula = pressC && args.includes("--with-nula");
cpu.writemem(0x7d, variant === "nula" || withNula ? 0xff : 0);
cpu.writemem(0x7e, withNula ? 0x80 : pressC ? 0 : ({ nula: 0x80, cpc: 0x40, cols: 0x40, cols2: 0x40 }[variant] ?? 0));
await s.runFor(1);
if (pressC) {
    // the intro polls the keys while its text is being drawn
    for (let t = 0; t < 30 && !(mem(0x7e) & 0x40); t++) {
        s.keyDown("KeyC");
        await s.runFor(400_000);
        s.keyUp("KeyC");
        await s.runFor(400_000);
    }
    console.log(`--press-c: &7E = &${mem(0x7e).toString(16)} after C`);
    // --then-n (with --with-nula, --variant nula): N must switch NuLA back on and the CPC version off
    if (withNula && args.includes("--then-n")) {
        for (let t = 0; t < 30 && !(mem(0x7e) & 0x80); t++) {
            s.keyDown("KeyN");
            await s.runFor(400_000);
            s.keyUp("KeyN");
            await s.runFor(400_000);
        }
        console.log(`--then-n: &7E = &${mem(0x7e).toString(16)} after N`);
    }
    const introShot = opt("--intro-shot");
    if (introShot) {
        await s.runFor(2 * 39936);
        writeFileSync(introShot, await s.screenshot());
    }
}
// Second &1100 is the release LOW: replace it with the candidate.
await runUntilPc(0x1100, 400_000_000);
if (syms.main !== 0x1100) throw new Error("candidate main is not at &1100");
if (args.includes("--no-inject")) {
    // Run the LOW the disc loaded; check it is the one just assembled.
    let same = true;
    for (let i = 0; i < lowBin.length; i++) if (mem(0x1100 + i) !== lowBin[i]) same = false;
    console.log(`--no-inject: ${VARIANTS[variant].exe} from the disc ${same ? "matches" : "DIFFERS FROM"} the one assembled`);
    if (!same) process.exit(2);
} else {
    for (let i = 0; i < lowBin.length; i++) cpu.writemem(0x1100 + i, lowBin[i]);
}
const bootSecs = (Date.now() - t0) / 1000;

// --slow-disc ms: every seek's settle (and so every track read) takes this much
// longer, as on a slower drive than jsbeeb's: a test that the decoder never
// outruns the loader (frames must still match, only taking more fields).
const slowDisc = Number(opt("--slow-disc", "0"));
if (slowDisc) {
    const fdc = cpu.fdc;
    const set = fdc._setTimerMs.bind(fdc);
    fdc._setTimerMs = (state, ms) => set(state, state === 2 ? ms + slowDisc : ms); // 2: postSeek
}

// ---------------------------------------------------------------- measure

const irqVec = mem(0xfffe) | (mem(0xffff) << 8);
const nmiVec = mem(0xfffa) | (mem(0xfffb) << 8);
const excluded = (pc) =>
    pc >= 0x8000 || // SWRAM music, MOS, DFS ROM
    (pc >= 0x0d00 && pc < 0x0e00) || // NMI handler
    (pc >= syms.irq_handler && pc < syms.swap_frame_buffers); // our IRQ handler shell

const frames = [];
const profile = profilePath ? new Float64Array(65536) : null;
const spansPath = opt("--spans");
const spansFrames = Number(opt("--spans-frames", "3"));
const spans = spansPath ? [] : null;
let rendering = false;
let renderCycles = 0;
let polyCount = 0;
let prevPc = -1;
let prevCycles = 0;
let finished = false;

if (streamLog) {
    // the 8271's timers during each read (seek step, settle / head load, ...), and its commands
    const fdc = cpu.fdc;
    const setTimerMs = fdc._setTimerMs.bind(fdc);
    fdc._setTimerMs = (state, ms) => {
        if (loadOpen) (loadOpen.timers[state] ??= [0, 0]), loadOpen.timers[state][0]++, (loadOpen.timers[state][1] += ms);
        return setTimerMs(state, ms);
    };
    fdc._logCommands = true;
    fdc._log = (m) => { if (loadOpen) loadOpen.fdcLog.push(m); };
}

// --timeline: every cycle from the first frame's decode to the end, by what the
// machine was doing; the decoder's idle time split by what it waits for.
const timeline = args.includes("--timeline") ? {} : null;
let timelineOn = false;
function timelineCategory(pc) {
    if (pc >= 0x8000 && pc < 0xc000 && mem(0xf4) === mem(0x7f)) return "music player (SWRAM)";
    if (pc >= 0x0d00 && pc < 0x0e00) return "disc NMI handler";
    if (rendering) return excluded(pc) ? "interrupt overhead during render" : "render";
    if (pc >= syms.irq_handler && pc < syms.swap_frame_buffers) return "IRQ handler";
    if (mem(syms.screen_lock)) return "idle: frame done, waiting for vsync to swap";
    if (mem(syms.buffer_lock)) return "idle: waiting for stream data";
    return "idle: waiting for the Timer 1 tick to start a frame";
}

// --trap-wild: stop when the CPU enters &8000-&BFFF other than from the IRQ
// handler (the music call) or the MOS, and print the instructions that led there.
const trapWild = args.includes("--trap-wild");

// --shot N file (repeatable): a screenshot of frame N on screen
const shots = [];
for (let i = 0; i < args.length; i++) if (args[i] === "--shot") shots.push([Number(args[i + 1]), args[i + 2]]);
shots.sort((a, b) => a[0] - b[0]);
let shotDue = false;
const trail = [];

const hook = cpu.debugInstruction.add((pc) => {
    const now = s.elapsedCycles;
    if (trapWild && timelineOn) {
        trail.push(pc);
        if (trail.length > 64) trail.shift();
        if (pc >= 0x8000 && pc < 0xc000 && prevPc < 0x8000 && !(prevPc >= syms.irq_handler && prevPc < syms.swap_frame_buffers) && prevPc !== irqVec && !(prevPc >= 0x0d00 && prevPc < 0x0e00)) {
            console.log(`wild jump to ${pc.toString(16)} in frame ${frames.length}: ` + trail.map((x) => x.toString(16)).join(" "));
            process.exit(2);
        }
    }
    if (timeline && timelineOn && prevPc >= 0) {
        const c = timelineCategory(prevPc);
        timeline[c] = (timeline[c] ?? 0) + (now - prevCycles);
    }
    if (rendering && prevPc >= 0 && !excluded(prevPc)) {
        let d = now - prevCycles;
        if (pc === irqVec || pc === nmiVec) d -= 7;
        renderCycles += d;
        if (profile) profile[prevPc] += d;
    }
    prevPc = pc;
    prevCycles = now;

    if (streamLog) {
        const locked = mem(syms.buffer_lock) !== 0;
        if (locked && lockOn === null) lockOn = now;
        else if (!locked && lockOn !== null) {
            streamLog.locks.push([lockOn, now]);
            lockOn = null;
        }
        if (pc === syms.load_next_track && mem(syms.track_no) & 0x80) {
            // no more to read: returns at once
        } else if (pc === syms.load_next_track) {
            loadOpen = { start: now, drive: mem(syms.osword_params_drive), track: mem(syms.track_no), slot: mem(syms.load_to_HI),
                timers: {}, fdcLog: [], regs: Array.from(cpu.fdc._regs.slice(0x0d, 0x20)) };
        } else if (loadOpen && loadReturns.has(pc)) {
            loadOpen.end = now;
            loadOpen.error = mem(syms.error_flag);
            streamLog.loads.push(loadOpen);
            loadOpen = null;
        } else if (pc === syms.parse_frame) {
            streamLog.frames.push([now, mem(syms.STREAM_ptr_HI), mem(syms.STREAM_ptr_LO)]);
        }
    }
    if (pc === syms.parse_frame) {
        if (timeline || trapWild) timelineOn = true;
        rendering = true;
        renderCycles = 0;
        polyCount = 0;
    } else if (pc === syms.plot_poly_span) {
        polyCount++;
    } else if (pc === parseReturn && rendering) {
        rendering = false;
        // A = parse_frame's return: &FD on the last frame of the stream
        frames.push({ render: renderCycles, last: cpu.a === 0xfd, polys: polyCount });
    } else if (pc === syms.swap_frame_buffers) {
        const hi = mem(syms.draw_buffer_HI);
        const buf = new Uint8Array(6400);
        for (let i = 0; i < 6400; i++) buf[i] = mem((hi << 8) + i);
        // LOW: the 16 ULA palette registers; NULA: the 16 colours they render through the NuLA
        const pal = variant === "nula" ? new Uint8Array(Uint32Array.from(s._video.ulaPal.slice(0, 16)).buffer) : Uint8Array.from(s._video.actualPal.slice(0, 16));
        const f = frames[frames.length - 1];
        f.hash = createHash("sha1").update(buf).update(pal).digest("hex");
        f.buffer = hi;
        if (dumpDir) {
            mkdirSync(dumpDir, { recursive: true });
            writeFileSync(path.join(dumpDir, `f${String(frames.length - 1).padStart(4, "0")}.bin`), Buffer.concat([buf, pal]));
        }
        if (f.last) {
            finished = true;
            return true;
        }
        if (shots.length && frames.length - 1 === shots[0][0]) {
            shotDue = true;
            return true;
        }
    } else if (pc === syms.track_load_error) {
        // The main loop leaves on a disc error (or end of stream) without the last frame.
        finished = true;
        return true;
    } else if (spans && pc === syms.fast_plot_spans && frames.length < spansFrames) {
        // Span buffer for each polygon as the edge stage left it: [frame, ymin, [[start, end], ...]]
        const lo = mem(syms.span_buffer_min_y);
        const hi = mem(syms.span_buffer_max_y);
        const rows = [];
        for (let y = lo; y <= hi; y++) rows.push([mem(syms.span_buffer_start + y), mem(syms.span_buffer_end + y)]);
        const verts = [];
        for (let i = 0; i < mem(syms.poly_num_verts); i++) verts.push([mem(syms.poly_verts_x + i), mem(syms.poly_verts_y + i)]);
        spans.push([frames.length, lo, rows, verts]);
    }
});

const runStart = s.elapsedCycles;
for (let budget = 2_000_000 * 900; ; ) {
    const before = s.elapsedCycles;
    await s.runFor(budget);
    budget -= s.elapsedCycles - before;
    if (!shotDue || budget <= 0) break;
    // --shot: the frame just drawn is shown at the next vsync; a field later, capture it
    shotDue = false;
    const [n, file] = shots.shift();
    const t = s.elapsedCycles;
    await s.runFor(2 * 39936);
    budget -= s.elapsedCycles - t;
    writeFileSync(file, await s.screenshot());
    console.log(`shot: frame ${n} -> ${file}`);
}
hook.remove();
if (streamLog) writeFileSync(streamLogPath, JSON.stringify(streamLog));
if (!finished) {
    const st = ["track_no", "osword_params_drive", "error_flag", "buffer_lock", "load_to_HI"].map((k) => `${k}=${mem(syms[k]).toString(16)}`);
    throw new Error(`stream did not finish; ${frames.length} frames; pc=${cpu.pc.toString(16)} ${st.join(" ")} ptr=${mem(1).toString(16)}${mem(0).toString(16).padStart(2, "0")}`);
}
// The main loop sees eof_flag and copies vsync_counter to vsync_final within a field or
// so, or once a track read it is in the middle of returns (the column disc's stream ends
// before the disc does, and the loader reads on).
await s.runFor(200_000);
for (let t = 0; t < 20 && !(mem(syms.vsync_final) | mem(syms.vsync_final + 1)); t++) await s.runFor(200_000);

const vsyncFinal = mem(syms.vsync_final) | (mem(syms.vsync_final + 1) << 8);
const errorFlag = mem(syms.error_flag);
const totalRender = frames.reduce((a, f) => a + f.render, 0);
const result = {
    frames: frames.length,
    total_render_cycles: totalRender,
    vsync_final: vsyncFinal,
    error_flag: errorFlag,
    wall_cycles: s.elapsedCycles - runStart,
    per_frame: frames.map((f) => ({ render: f.render, hash: f.hash, polys: f.polys })),
};

console.log(`boot ${bootSecs.toFixed(1)}s, run ${((Date.now() - t0) / 1000 - bootSecs).toFixed(1)}s host time`);
if (errorFlag) console.log(`WARNING: stream stopped on disc error &${errorFlag.toString(16)} after ${frames.length} frames`);
console.log(`frames ${result.frames}  render cycles ${totalRender}  (${(totalRender / 2e6).toFixed(3)} s)  vsync_final ${vsyncFinal} fields`);

if (refPath && existsSync(refPath)) {
    const ref = JSON.parse(readFileSync(refPath));
    let bad = 0;
    const n = Math.max(ref.per_frame.length, frames.length);
    for (let i = 0; i < n; i++) {
        if (ref.per_frame[i]?.hash !== frames[i]?.hash) {
            if (bad < 10) console.log(`MISMATCH frame ${i}`);
            bad++;
        }
    }
    const delta = totalRender - ref.total_render_cycles;
    console.log(
        bad === 0
            ? `PASS: all ${n} frames identical; render ${delta >= 0 ? "+" : ""}${delta} cycles (${((100 * delta) / ref.total_render_cycles).toFixed(2)}%), fields ${vsyncFinal - ref.vsync_final >= 0 ? "+" : ""}${vsyncFinal - ref.vsync_final}`
            : `FAIL: ${bad} of ${n} frames differ`,
    );
    result.pass = bad === 0;
}
if (outPath) writeFileSync(outPath, JSON.stringify(result, null, 1));
if (spans) writeFileSync(spansPath, JSON.stringify(spans));
if (streamLog) writeFileSync(streamLogPath, JSON.stringify(streamLog));
if (timeline) {
    const total = Object.values(timeline).reduce((a, b) => a + b, 0);
    console.log(`timeline: ${total} cycles (${(total / 39936).toFixed(0)} fields) from the first frame's decode to the last swap`);
    for (const [c, v] of Object.entries(timeline).sort((a, b) => b[1] - a[1]))
        console.log(`  ${c.padEnd(52)} ${String(v).padStart(11)} ${(v / 39936).toFixed(0).padStart(6)} fields ${((100 * v) / total).toFixed(1).padStart(5)}%`);
}
if (profile) {
    // Per-instruction render cycles, then summed under the nearest preceding label in the listing.
    const lst = readFileSync(path.join(src, "build", `${variant}.txt`), "utf8").split("\n");
    const labelAt = [];
    let label = "?";
    for (const line of lst) {
        const l = line.match(/^\.(\^?[A-Za-z_][A-Za-z0-9_]*)/);
        if (l) label = l[1];
        const a = line.match(/^\s+([0-9A-F]{4})\s{3}/);
        if (a) labelAt[parseInt(a[1], 16)] ??= label;
    }
    const byLabel = {};
    const byPc = [];
    for (let pc = 0; pc < 65536; pc++) {
        if (!profile[pc]) continue;
        byPc.push([pc, profile[pc]]);
        const l = labelAt[pc] ?? `?${pc.toString(16)}`;
        byLabel[l] = (byLabel[l] ?? 0) + profile[pc];
    }
    const out = ["label cycles %"];
    for (const [l, c] of Object.entries(byLabel).sort((a, b) => b[1] - a[1]))
        out.push(`${l} ${c} ${((100 * c) / totalRender).toFixed(2)}`);
    out.push("", "pc cycles %");
    for (const [pc, c] of byPc.sort((a, b) => b[1] - a[1]).slice(0, Number(opt("--profile-pcs", "300"))))
        out.push(`${pc.toString(16)} ${c} ${((100 * c) / totalRender).toFixed(2)} ${labelAt[pc] ?? ""}`);
    writeFileSync(profilePath, out.join("\n"));
}
// --outro dir: run on after the stream for --outro-secs (default 60) seconds of
// machine time, saving a screenshot every 2 s (outro.NNN.png) to see what follows.
// --outro-dump dir (cols2): play on into the outro and save each of its frames'
// window, 100 lines of 16 bytes (o0000.bin...), for tools/outro_check.py.
const outroDump = opt("--outro-dump");
if (outroDump) {
    const o = VARIANTS[variant].outro;
    if (!o) throw new Error(`--outro-dump: no outro build for --variant ${variant}`);
    const out = execFileSync(beebasm, [...o.defines.flatMap((d) => ["-D", d]), "-i", "src/outro.asm", "-d", "-v"], { cwd: src, maxBuffer: 64 << 20 }).toString();
    const osyms = {};
    for (const m of out.matchAll(/'([A-Za-z_0-9]+)':(-?\d+)L/g)) osyms[m[1]] = Number(m[2]);
    const bin = readFileSync(path.join(src, "build", o.exe));
    let ret = -1;
    for (let i = osyms.irq_handler - 0x1100; i < bin.length - 2 && ret < 0; i++)
        if (bin[i] === 0x20 && bin[i + 1] === (osyms.parse_frame & 0xff) && bin[i + 2] === osyms.parse_frame >> 8) ret = 0x1100 + i + 3;
    if (ret < 0) throw new Error("--outro-dump: jsr parse_frame not found in the outro's irq_handler");
    mkdirSync(outroDump, { recursive: true });
    let n = 0;
    let first = 0;
    let lastCycle = 0;
    let done = false;
    // --outro-profile f.txt: cycles by instruction from the outro's parse_frame to
    // its return, leaving out interrupt handlers (as render cycles are counted)
    const oprofPath = opt("--outro-profile");
    const oprof = oprofPath ? new Float64Array(65536) : null;
    let inFrame = false;
    let oPrev = -1;
    let oPrevCycles = 0;
    let oRender = 0;
    const oh = cpu.debugInstruction.add((pc) => {
        const now = s.elapsedCycles;
        if (inFrame && oPrev >= 0 && ((oPrev >= osyms.fx_start && oPrev < osyms.get_char_def) || (oPrev >= 0x5a40 && oPrev < 0x5c00))) {
            let d = now - oPrevCycles;
            if (pc === irqVec || pc === nmiVec) d -= 7;
            oRender += d;
            if (oprof) oprof[oPrev] += d;
        }
        oPrev = pc;
        oPrevCycles = now;
        if ((pc === osyms.parse_frame || pc === 0x5a40) && mem(0x1100) === bin[0] && mem(0x1101) === bin[1]) inFrame = true;
        // the outro is the code at &1100 now: its frame's return, in its IRQ handler
        if (pc !== ret || mem(0x1100) !== bin[0] || mem(0x1101) !== bin[1]) return;
        inFrame = false;
        const hi = mem(osyms.draw_buffer_HI);
        const win = new Uint8Array(100 * 16);
        for (let y = 0; y < 100; y++) {
            const line = 8 + y;
            for (let k = 0; k < 16; k++) win[y * 16 + k] = mem((hi << 8) + (line >> 3) * 320 + 96 + k * 8 + (line & 7));
        }
        writeFileSync(path.join(outroDump, `o${String(n).padStart(4, "0")}.bin`), win);
        if (n === 0) first = s.elapsedCycles;
        lastCycle = s.elapsedCycles;
        n++;
        if (cpu.a === 0xfd) {
            done = true;
            return true;
        }
    });
    for (let budget = 2_000_000 * 400; budget > 0 && !done; budget -= 20_000_000) await s.runFor(20_000_000);
    oh.remove();
    console.log(`outro: ${n} frames dumped in ${outroDump}${done ? "" : " (did not reach the end)"}; ` +
        `${((lastCycle - first) / 2e6).toFixed(1)} s from the first to the last (${((lastCycle - first) / 39936).toFixed(0)} fields); ` +
        `${(oRender / 1e6).toFixed(1)}M cycles in parse_frame (${(oRender / Math.max(n, 1) / 1000).toFixed(0)}K a frame)`);
    if (oprof) {
        const out = ["pc cycles"];
        for (let pc = 0; pc < 65536; pc++) if (oprof[pc]) out.push(`${pc.toString(16).padStart(4, "0")} ${oprof[pc]}`);
        writeFileSync(oprofPath, out.join("\n"));
    }
}

const outroDir = opt("--outro");
if (outroDir) {
    mkdirSync(outroDir, { recursive: true });
    const secs = Number(opt("--outro-secs", "60"));
    for (let t = 0; t < secs; t += 2) {
        await s.runFor(4_000_000);
        writeFileSync(path.join(outroDir, `outro.${String(t + 2).padStart(3, "0")}.png`), await s.screenshot());
    }
    console.log(`outro: ${secs / 2} screenshots in ${outroDir}; pc=${cpu.pc.toString(16)}`);
}
s.destroy();
process.exit(result.pass === false ? 1 : 0);
