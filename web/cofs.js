"use strict";
// Circle of Fifths -- TypeScript port of src/Lib.hs for the browser.
// Geometry follows diagrams' conventions (y up, angles in turns,
// counter-clockwise) and is mapped onto an 800x800 SVG like
// `renderSVG "test.svg" (mkWidth 800)` does: 40 px per unit, origin at
// (400, 400).
//
// The circle works like a volvelle: the keys spiral is the fixed base, the
// solfege ring (with the modes) turns on it, and the steps ring turns on the
// solfege ring. Both positions are counted in fifths and are fractional
// while a ring is being dragged.
// Stops (see ToDo): the colored solfege wedges must stay within the keys
// spiral (C♭ .. E♯), and the I of the steps ring within the colored wedges.
const solfegeDoRange = [-6, 6];
const tonicRange = [-1, 5];
// Ring layout ------------------------------------------------------------
// The rings are stacked from the hub outwards. Each ring holds one or more
// rows of text; its radii follow from the row heights (= font sizes), the
// padding towards its edges and the gap between the rings.
const hubRadius = 2.6;
const ringGap = 0.12;
const padding = 0.25;
const rowGap = 0.2;
const keyFont = 0.8;
const solfegeFont = 0.9;
const modeFont = 0.5;
const stepFont = 0.8;
// The keys form a spiral: one full turn (12 fifths) moves a key outwards
// by this much, so enharmonic keys (E♯/F, C♭/B, ...) share a wedge without
// touching.
const spiralTurn = 1.25 * keyFont;
const spiralDelta = spiralTurn / 12;
// Stack a ring with the given rows (font sizes, inside out) onto `inner`.
const stackRing = (inner, fonts, extra = 0) => {
    const rows = [];
    let r = inner + padding;
    fonts.forEach((f, i) => {
        if (i > 0)
            r += rowGap;
        rows.push(r + f / 2);
        r += f;
    });
    return { inner, outer: r + extra + padding, rows };
};
// The keys ring is one row whose radius grows by 18 steps along the spiral.
const keysLayout = stackRing(hubRadius, [keyFont], 18 * spiralDelta);
const solfegeLayout = stackRing(keysLayout.outer + ringGap, [solfegeFont, modeFont]);
const stepsLayout = stackRing(solfegeLayout.outer + ringGap, [stepFont]);
const size = 800;
const scale = size / 20; // the diagram is bounded by `circle 10`
// Turn of the whole circle (in turns, counter-clockwise): 1/2 puts C at
// the bottom.
const baseTurn = 1 / 2;
const lineWidth = 3.2; // diagrams' default line width at this size
const black = "rgb(0,0,0)";
const gray = "rgb(128,128,128)";
const white = "rgb(255,255,255)";
const honeydew = "rgb(240,255,240)";
const bisque = "rgb(255,228,196)";
const pink = "rgb(255,192,203)";
// Haskell's `mod` and `div` round towards negative infinity.
const mod = (a, b) => ((a % b) + b) % b;
const div = (a, b) => Math.floor(a / b);
const clamp = ([lo, hi], x) => Math.min(hi, Math.max(lo, x));
const fmt = (x) => (Math.abs(x) < 5e-5 ? 0 : x).toFixed(4);
// A point at radius r and angle t (in turns) as SVG coordinates.
const point = (r, t) => [
    size / 2 + scale * r * Math.cos(2 * Math.PI * t),
    size / 2 - scale * r * Math.sin(2 * Math.PI * t),
];
// Every curved label needs its own path; ids are numbered per rendering.
let pathCount = 0;
// Text centred at radius r and angle t + 1/4 (t in turns, as `rotateBy t .
// translateY r` in Lib.hs), set along a clockwise arc so that it reads from
// left to right with its top pointing outwards -- it turns with its ring.
const key = (s, r, t, fill, fontSize, opacity = 1) => {
    const id = `arc${pathCount++}`;
    const c = t + 1 / 4;
    const rb = r - 0.33 * fontSize; // baseline, so that capitals are centred on r
    const [x0, y0] = point(rb, c + 1 / 12);
    const [x1, y1] = point(rb, c - 1 / 12);
    const R = fmt(scale * rb);
    // The ♭ and ♯ come from a fallback font whose side bearings are too wide.
    const [, base, accidentals] = s.match(/^(.*?)([♭♯]*)$/);
    const body = accidentals ? `${base}<tspan dx="-0.08em">${accidentals}</tspan>` : s;
    const alpha = opacity < 1 ? ` opacity="${fmt(opacity)}"` : "";
    return (`<path id="${id}" d="M ${fmt(x0)},${fmt(y0)} A ${R},${R} 0 0 1 ${fmt(x1)},${fmt(y1)}"` +
        ` fill="none" stroke="none"/>` +
        `<text fill="${fill}" font-size="${fmt(fontSize * scale)}px"${alpha}>` +
        `<textPath href="#${id}" startOffset="50%">${body}</textPath></text>`);
};
// annularWedge outer inner: a ring segment from t0 to t1 (in turns).
const annularWedge = (outer, inner, t0, t1, fill) => {
    const [ox0, oy0] = point(outer, t0);
    const [ox1, oy1] = point(outer, t1);
    const [ix1, iy1] = point(inner, t1);
    const [ix0, iy0] = point(inner, t0);
    const R = fmt(scale * outer);
    const r = fmt(scale * inner);
    const d = `M ${fmt(ox0)},${fmt(oy0)} A ${R},${R} 0 0 0 ${fmt(ox1)},${fmt(oy1)}` +
        ` L ${fmt(ix1)},${fmt(iy1)} A ${r},${r} 0 0 1 ${fmt(ix0)},${fmt(iy0)} Z`;
    return fill ? `<path d="${d}" fill="${fill}"/>` : `<path d="${d}" fill="none"/>`;
};
const range = (n) => Array.from({ length: n }, (_, i) => i);
// Ring of Keys; Spiral of 19 Keys in Wedges ------------------------------
const alterateKey = (n) => {
    if (n < -1)
        return "♭♭" + alterateKey(n + 2); // '𝄫'
    if (n < 0)
        return "♭".repeat(-n);
    if (n > 1)
        return "♯♯" + alterateKey(n - 2); // '𝄪'
    return "♯".repeat(n);
};
// The fixed spiral from C♭ to E♯, i.e. `keyText (0, _)` in Lib.hs.
const keyText = range(19)
    .map((i) => i - 8)
    .map((n) => "ABCDEFG"[mod(4 * n - 1, 7)] + alterateKey(div(n + 2, 7)));
// The keys under the colored solfege wedges (Fa .. TI) are active; the
// others are grayed out once the ring is released.
const keysRing = ({ solfegeDo, dragging }) => {
    const fa = Math.round(solfegeDo) + 6;
    return keyText.map((s, n) => {
        const active = dragging || (n >= fa && n < fa + 7);
        return key(s, keysLayout.rows[0] + n * spiralDelta, 8 / 12 - (n + 1) / 12, active ? black : gray, keyFont);
    });
};
const keyWedges = () => range(12).map((i) => annularWedge(keysLayout.outer, keysLayout.inner, i / 12 - 1 / 24, i / 12 + 1 / 24));
// -------------------------------------------------------------------------
// Ring Solfege; 7 Solfege Syllables in colored wedges (3 major, 3 minor, 1 diminished)
// could also be prolongued with "Fi", "Di" and so on
const solfegeText = ["Fa", "Do", "So", "re", "la", "mi", "TI"];
const modesText = ["Lydian", "Ionian", "Mixolydian", "Dorian", "Aeolian", "Phrygian", "Locrian"];
// The syllable and mode of the tonic are black, the others gray.
const ringColors = (tonic) => range(7).map((k) => (k === Math.round(tonic) + 1 ? black : gray));
// Rotation of the solfege ring at position k (0 = Fa .. 6 = TI); k may be
// fractional for the steps ring turning on it.
const ringTurn = (solfegeDo, k) => (2 - solfegeDo) / 12 - (k + 1) / 12;
const solfegeRing = ({ solfegeDo, tonic }) => {
    const cs = ringColors(tonic);
    return solfegeText.map((s, k) => key(s, solfegeLayout.rows[0], ringTurn(solfegeDo, k), cs[k], solfegeFont));
};
const modesRing = ({ solfegeDo, tonic }) => {
    const cs = ringColors(tonic);
    return modesText.map((s, k) => key(s, solfegeLayout.rows[1], ringTurn(solfegeDo, k), cs[k], modeFont));
};
const solfegeWedges = ({ solfegeDo }) => {
    const solfegeColors = [honeydew, honeydew, honeydew, bisque, bisque, bisque, pink];
    return solfegeColors.map((c, k) => {
        const t = 1 / 4 - 1 / 24 + ringTurn(solfegeDo, k);
        return annularWedge(solfegeLayout.outer, solfegeLayout.inner, t, t + 1 / 12, c);
    });
};
// -------------------------------------------------------------------------
// Ring Steps; 13 Steps in 12 Wedges, counted in fifths from the tonic
// (-6 .. 6; ♭V and ♯IV share a wedge). Only the steps over the colored
// solfege wedges are shown, in the case of the chord found there.
const showRoman = (x) => {
    if (x < 1 || x > 7)
        throw new Error("showRoman only defined for 1..7!");
    return ["I", "II", "III", "IV", "V", "VI", "VII"][x - 1];
};
const chordCase = [
    (s) => s,
    (s) => s,
    (s) => s,
    (s) => s.toLowerCase(),
    (s) => s.toLowerCase(),
    (s) => s.toLowerCase(),
    (s) => s.toLowerCase() + " dim",
];
const stepsRing = ({ solfegeDo, tonic }) => range(13)
    .map((i) => i - 6)
    .flatMap((o) => {
    const k = tonic + 1 + o; // position over the solfege ring
    const outside = Math.max(-0.5 - k, k - 6.5, 0);
    const opacity = 1 - 2 * outside; // fade out beyond the colored wedges
    if (opacity <= 0)
        return [];
    const s = chordCase[clamp([0, 6], Math.round(k))](showRoman(mod(4 * o, 7) + 1));
    return [key(s, stepsLayout.rows[0], ringTurn(solfegeDo, k), black, stepFont, opacity)];
});
const stepWedges = ({ solfegeDo }) => range(7).map((k) => {
    const t = 1 / 4 - 1 / 24 + ringTurn(solfegeDo, k);
    return annularWedge(stepsLayout.outer, stepsLayout.inner, t, t + 1 / 12);
});
// -------------------------------------------------------------------------
// Layers from bottom to top, i.e. the reverse of the `<>` chain in Lib.hs.
const cofs = (state) => {
    pathCount = 0;
    return [
        `<svg xmlns="http://www.w3.org/2000/svg" width="${size}" height="${size}"` +
            ` viewBox="0 0 ${size} ${size}" stroke="${black}" stroke-width="${lineWidth}"` +
            ` font-family="'Latin Modern Roman', 'Latin Modern Math', 'CMU Serif', serif"` +
            ` text-anchor="middle">`,
        `<rect width="${size}" height="${size}" fill="${white}" stroke="none"/>`,
        `<g transform="rotate(${fmt(-360 * baseTurn)} ${size / 2} ${size / 2})">`,
        ...stepWedges(state),
        `<g stroke="none">`,
        ...stepsRing(state),
        `</g>`,
        ...solfegeWedges(state),
        `<g stroke="none">`,
        ...modesRing(state),
        ...solfegeRing(state),
        `</g>`,
        ...keyWedges(),
        `<g stroke="none">`,
        ...keysRing(state),
        `</g>`,
        `<circle cx="${size / 2}" cy="${size / 2}" r="${0.1 * scale}" fill="none"/>`,
        `</g>`,
        `</svg>`,
    ].join("\n");
};
// The mode numbers of Lib.hs (1 Ionian, 2 Dorian, ... 7 Locrian) and the
// tonic position in fifths from Do.
const modeOfTonic = (tonic) => mod(4 * (tonic + 2) - 1, 7) + 1;
const tonicOfMode = (mode) => mod(2 * mode - 1, 7) - 1;
const describe = ({ solfegeDo, tonic }) => {
    const d = Math.round(solfegeDo);
    const t = Math.round(tonic);
    return `${keyText[d + 7 + t]} ${modesText[t + 1]}`;
};
// -------------------------------------------------------------------------
// The browser counterpart of `loop`: (+) Step up. (-) Step down. (m) Cycle
// mode. In addition the solfege and steps rings can be dragged, and a click
// on a mode selects it.
const initialize = () => {
    let state = { solfegeDo: 0, tonic: 0, dragging: false };
    const target = document.getElementById("cofs");
    const status = document.getElementById("state");
    const draw = () => {
        target.innerHTML = cofs(state);
        status.textContent = describe(state);
    };
    // Let the rings glide into place.
    let frame = 0;
    const animateTo = (solfegeDo, tonic) => {
        cancelAnimationFrame(frame);
        const from = { ...state };
        const start = performance.now();
        const duration = 250;
        const step = (now) => {
            const u = Math.min(1, (now - start) / duration);
            const e = 1 - (1 - u) ** 3;
            state = {
                ...state,
                solfegeDo: from.solfegeDo + (solfegeDo - from.solfegeDo) * e,
                tonic: from.tonic + (tonic - from.tonic) * e,
            };
            draw();
            if (u < 1)
                frame = requestAnimationFrame(step);
        };
        frame = requestAnimationFrame(step);
    };
    const actions = {
        "+": () => animateTo(clamp(solfegeDoRange, Math.round(state.solfegeDo) + 1), state.tonic),
        "-": () => animateTo(clamp(solfegeDoRange, Math.round(state.solfegeDo) - 1), state.tonic),
        m: () => animateTo(state.solfegeDo, tonicOfMode(mod(modeOfTonic(Math.round(state.tonic)), 7) + 1)),
    };
    const act = (c) => actions[c]?.();
    document.addEventListener("keydown", (e) => act(e.key));
    document.querySelectorAll("button[data-key]").forEach((b) => b.addEventListener("click", () => act(b.dataset.key)));
    // Pointer position as radius and angle (in turns) in diagram units,
    // relative to the turned circle.
    const polar = (e) => {
        const b = target.firstElementChild.getBoundingClientRect();
        const x = ((e.clientX - b.left) / b.width - 0.5) * (size / scale);
        const y = (0.5 - (e.clientY - b.top) / b.height) * (size / scale);
        return [Math.hypot(x, y), Math.atan2(y, x) / (2 * Math.PI) - baseTurn];
    };
    const ringAt = (r) => r >= solfegeLayout.inner && r <= solfegeLayout.outer
        ? "solfege"
        : r >= stepsLayout.inner && r <= stepsLayout.outer
            ? "steps"
            : null;
    let drag = null;
    target.addEventListener("pointerdown", (e) => {
        const [r, turn] = polar(e);
        const ring = ringAt(r);
        if (!ring)
            return;
        cancelAnimationFrame(frame);
        drag = { ring, turn, x: e.clientX, y: e.clientY, moved: false };
        target.setPointerCapture(e.pointerId);
    });
    target.addEventListener("pointermove", (e) => {
        if (!drag) {
            target.style.cursor = ringAt(polar(e)[0]) ? "grab" : "";
            return;
        }
        if (!drag.moved && Math.hypot(e.clientX - drag.x, e.clientY - drag.y) < 4)
            return;
        drag.moved = true;
        target.style.cursor = "grabbing";
        const [, turn] = polar(e);
        const d = turn - drag.turn;
        drag.turn = turn;
        // Turning clockwise (negative angle) moves the ring up by fifths.
        const fifths = -12 * (d - Math.round(d));
        state =
            drag.ring === "solfege"
                ? { ...state, dragging: true, solfegeDo: clamp(solfegeDoRange, state.solfegeDo + fifths) }
                : { ...state, dragging: true, tonic: clamp(tonicRange, state.tonic + fifths) };
        draw();
    });
    const release = (e) => {
        if (!drag)
            return;
        if (drag.moved) {
            state = { ...state, dragging: false };
            animateTo(Math.round(state.solfegeDo), Math.round(state.tonic));
        }
        else if (drag.ring === "solfege") {
            // A click on a syllable or mode makes it the tonic.
            const [, turn] = polar(e);
            const k = mod(Math.round(1 - state.solfegeDo - 12 * (turn - 1 / 4)), 12);
            if (k <= 6)
                animateTo(state.solfegeDo, k - 1);
        }
        target.style.cursor = "";
        drag = null;
    };
    target.addEventListener("pointerup", release);
    target.addEventListener("pointercancel", release);
    document.getElementById("save").addEventListener("click", () => {
        const blob = new Blob([cofs(state)], { type: "image/svg+xml" });
        const a = document.createElement("a");
        a.href = URL.createObjectURL(blob);
        a.download = "cofs.svg";
        a.click();
        URL.revokeObjectURL(a.href);
    });
    draw();
};
if (typeof document !== "undefined")
    initialize();
