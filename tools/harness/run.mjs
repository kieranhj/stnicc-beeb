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
const disc = path.resolve(repo, opt("--disc", "build/beeb-niccc.dsd"));
const lowPath = path.join(repo, "build", "LOW");
const refPath = opt("--ref");
const outPath = opt("--out");
const dumpDir = opt("--dump");
const profilePath = opt("--profile");

// ---------------------------------------------------------------- build + symbols

const parseOnly = args.includes("--parse-only");

function buildLow() {
    // -d dumps every symbol as 'name':valueL, comma separated
    const defines = ["-D", "_QUALITY=0", "-D", "_NULA=0"];
    if (parseOnly) defines.push("-D", "_TEST_PARSE_ONLY=-1");
    const out = execFileSync(beebasm, [...defines, "-i", "stnicc-beeb.asm", "-d", "-v"], {
        cwd: repo,
        maxBuffer: 64 << 20,
    }).toString();
    writeFileSync(path.join(repo, "build", "low.txt"), out);
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
    const lst = readFileSync(path.join(repo, "build", "low.txt"), "utf8");
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

// First &1100 is INTRO: clear the NuLA flags so it chains to /LOW, not /NULA.
await runUntilPc(0x1100, 120_000_000);
cpu.writemem(0x7d, 0);
cpu.writemem(0x7e, 0);
await s.runFor(1);
// Second &1100 is the release LOW: replace it with the candidate.
await runUntilPc(0x1100, 400_000_000);
if (syms.main !== 0x1100) throw new Error("candidate main is not at &1100");
if (args.includes("--no-inject")) {
    // Run the LOW the disc loaded; check it is the one just assembled.
    let same = true;
    for (let i = 0; i < lowBin.length; i++) if (mem(0x1100 + i) !== lowBin[i]) same = false;
    console.log(`--no-inject: LOW from the disc ${same ? "matches" : "DIFFERS FROM"} build/LOW`);
    if (!same) process.exit(2);
} else {
    for (let i = 0; i < lowBin.length; i++) cpu.writemem(0x1100 + i, lowBin[i]);
}
const bootSecs = (Date.now() - t0) / 1000;

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

const hook = cpu.debugInstruction.add((pc) => {
    const now = s.elapsedCycles;
    if (rendering && prevPc >= 0 && !excluded(prevPc)) {
        let d = now - prevCycles;
        if (pc === irqVec || pc === nmiVec) d -= 7;
        renderCycles += d;
        if (profile) profile[prevPc] += d;
    }
    prevPc = pc;
    prevCycles = now;

    if (pc === syms.parse_frame) {
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
        const pal = Uint8Array.from(s._video.actualPal.slice(0, 16));
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
await s.runFor(2_000_000 * 900);
hook.remove();
if (!finished) throw new Error(`stream did not finish; ${frames.length} frames`);
// The main loop sees eof_flag and copies vsync_counter to vsync_final within a field or so.
await s.runFor(200_000);

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
if (profile) {
    // Per-instruction render cycles, then summed under the nearest preceding label in the listing.
    const lst = readFileSync(path.join(repo, "build", "low.txt"), "utf8").split("\n");
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
    for (const [pc, c] of byPc.sort((a, b) => b[1] - a[1]).slice(0, 300))
        out.push(`${pc.toString(16)} ${c} ${((100 * c) / totalRender).toFixed(2)} ${labelAt[pc] ?? ""}`);
    writeFileSync(profilePath, out.join("\n"));
}
s.destroy();
process.exit(result.pass === false ? 1 : 0);
