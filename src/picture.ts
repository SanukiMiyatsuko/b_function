import p5 from "p5";
import { P5CanvasInstance } from "react-p5-wrapper";
import { MySketchProps } from "./App";
import { ONE, T, Z, equal, sanitize_plus_term } from "./code";
import { Scanner } from "./parse";

const DEFAULT_FRAME_RATE = 60;

export const sketch_input = (p: P5CanvasInstance<MySketchProps>) => {
    let input = "";
    let nodeSize = 60;
    let nodeDistance = 90;
    let update = true;

    p.setup = () => {
        p.createCanvas(0, 0);
        p.textAlign(p.CENTER, p.CENTER);
        p.frameRate(DEFAULT_FRAME_RATE);
    };

    p.updateWithProps = props => {
        const inp = input;
        const ns = nodeSize;
        const nd = nodeDistance;
        ({
            inputstr: input,
            headSize: nodeSize,
            headDistance: nodeDistance,
        } = props);
        if (
            inp !== input ||
            ns !== nodeSize ||
            nd !== nodeDistance
        ) {
            update = true;
        }
    };

    p.draw = () => {
        try {
            if (update) {
                const parsedInput = input ? new Scanner(input).parse_term() : null;
                if (parsedInput === null) {
                    p.resizeCanvas(0, 0);
                } else {
                    let seq = [[-1,0]]
                    const op = O_RTtoST(parsedInput);
                    if (op === null) {
                        p.resizeCanvas(330, 40);
                        p.fill(0);
                        p.textSize(20);
                        p.text("ヒドラで表示できない項です", 130, 15);
                    } else {
                        seq = seq.concat(op);
                        p.resizeCanvas(seqLeng(seq, nodeDistance), canvasdepth(seq, nodeDistance));
                        drawKurage(seq, p, nodeSize, nodeDistance);
                    }
                }
            }
        } catch (e) {
            console.error("Error in draw function:", e);
        }
    }
}

export const sketch_output = (p: P5CanvasInstance<MySketchProps>) => {
    let outPut: T = Z;
    let nodeSize = 60;
    let nodeDistance = 90;
    let update = true;

    p.setup = () => {
        p.createCanvas(0, 0);
        p.textAlign(p.CENTER, p.CENTER);
        p.frameRate(DEFAULT_FRAME_RATE);
    };

    p.updateWithProps = props => {
        const outp = outPut;
        const ns = nodeSize;
        const nd = nodeDistance;
        ({
            output: outPut,
            headSize: nodeSize,
            headDistance: nodeDistance,
        } = props);
        if (
            !equal(outp, outPut) ||
            ns !== nodeSize ||
            nd !== nodeDistance
        ) {
            update = true;
        }
    };

    p.draw = () => {
        try {
            if (update) {
                if (equal(outPut, Z)) {
                    p.resizeCanvas(0, 0);
                } else {
                    let seq = [[-1,0]]
                    const op = O_RTtoST(outPut);
                    if (op === null) {
                        p.resizeCanvas(330, 40);
                        p.fill(0);
                        p.textSize(20);
                        p.text("ヒドラで表示できない項です", 130, 15);
                    } else {
                        seq = seq.concat(op);
                        p.resizeCanvas(seqLeng(seq, nodeDistance), canvasdepth(seq, nodeDistance));
                        drawKurage(seq, p, nodeSize, nodeDistance);
                    }
                }
            }
        } catch (e) {
            console.error("Error in draw function:", e);
        }
    }
}

function canvasdepth(seq: number[][], nodeMargin: number): number {
    if (seq.length === 0) return 0;
    
    let depth = Array(seq.length);
    for (let i = 0; i < seq.length; ++i)
        depth[i] = seq[i][0] + 1;

    const aryMax = (a: number, b: number) => Math.max(a, b);
    const depthMax = depth.reduce(aryMax);

    return (0.5 + depthMax) * nodeMargin + 0.5 * nodeMargin;
}

function seqLeng(seq: number[][], nodeMargin: number): number {
    if (seq.length === 0) return 0;
    return (0.5 + seq.length) * nodeMargin;
}

function drawKurage(seq: number[][], q: p5, nodeRadius: number, nodeMargin: number) {
    if (seq.length === 0) return 0;

    let depth = Array(seq.length);
    for (let i = 0; i < seq.length; ++i)
        depth[i] = seq[i][0] + 1;

    const parent = (x: number) => {
        let p = x - 1;
        while (p >= 0 && depth[p] >= depth[x]) {
            --p;
        }
        return p;
    };

    const parent0 = [...Array(seq.length)].map((_, i) => parent(i));

    let leg = Array(seq.length);
    for (let i = 0; i < seq.length; ++i)
        leg[i] = seq[i][1];

    const aryMax = (a: number, b: number) => Math.max(a, b);
    const depthMax = depth.reduce(aryMax);

    for (let i = 0; i < seq.length; ++i) {
        const x1 = (0.5 + i) * nodeMargin;
        q.noFill();
        if (depth[i] === 0) {
            q.line(
                x1 + nodeRadius / 3,
                (0.5 + depthMax) * nodeMargin + nodeRadius / 3,
                x1 - nodeRadius / 3,
                (0.5 + depthMax) * nodeMargin - nodeRadius / 3
            );
            q.line(
                x1 + nodeRadius / 3,
                (0.5 + depthMax) * nodeMargin - nodeRadius / 3,
                x1 - nodeRadius / 3,
                (0.5 + depthMax) * nodeMargin + nodeRadius / 3
            );
        } else {
            q.circle(
                x1,
                (0.5 + depthMax - depth[i]) * nodeMargin,
                nodeRadius
            );
            q.fill(0);
            q.textSize(nodeRadius/1.5);
            q.text(
                leg[i],
                x1,
                (0.5 + depthMax - depth[i]) * nodeMargin,
            );
            const x2 = (0.5 + parent0[i]) * nodeMargin + (nodeRadius / 2);
            const y1 = (0.5 + depthMax - depth[i]) * nodeMargin + (nodeRadius / 2);
            const y2 = (0.5 + depthMax - depth[parent0[i]]) * nodeMargin;
            q.noFill();
            q.arc(
                x2,
                y1,
                (x1 - x2) * 2,
                (y2 - y1) * 2,
                0, 
                Math.PI/2
            );
            if (parent0[i] === 0) {
                q.line(
                    x2,
                    (0.5 + depthMax - depth[parent0[i]]) * nodeMargin,
                    (0.5 + parent0[i]) * nodeMargin,
                    y2,
                );
            }
        }
    }
    return 1;
}

function number_term(s: T): number | null {
    if (s.type === "zero") {
        return 0;
    } else if (s.type === "plus") {
        if (s.add.every((x) => equal(x, ONE))) return s.add.length;
        return null;
    } else {
        if (equal(s, ONE)) return 1;
        return null;
    }
}

function O_RTtoST(s: T): number[][] | null {
    if (s.type === "zero") {
        return [];
    } else if (s.type === "plus") {
        const a = s.add[0];
        const oa = O_RTtoST(a);
        if (oa === null) return null;
        const b = sanitize_plus_term(s.add.slice(1));
        const ob = O_RTtoST(b);
        if (ob === null) return null;
        return oa.concat(ob);
    } else {
        const a = number_term(s.sub);
        if (a === null) return null;
        const b = s.arg;
        const beta = [[0,a]];
        const ob = O_RTtoST(b);
        if (ob === null) return null;
        const gamma = [...ob.map((x) => [x[0] + 1, x[1]])];
        return beta.concat(gamma);
    }
}
