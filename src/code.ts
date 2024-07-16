import { Options, headname } from "./App";

export type ZT = { readonly type: "zero" };
export type AT = { readonly type: "plus", readonly add: PT[] };
export type PT = { readonly type: "psi", readonly sub: T, readonly arg: T };
export type T = ZT | AT | PT;

export const Z: ZT = { type: "zero" };
export const ONE: PT = { type: "psi", sub: Z, arg: Z };
export const OMEGA: PT = { type: "psi", sub: Z, arg: ONE };
export const LOMEGA: PT = { type: "psi", sub: ONE, arg: Z };

// オブジェクトの相等判定
export function equal(s: T, t: T): boolean {
    if (s.type === "zero") {
        return t.type === "zero";
    } else if (s.type === "plus") {
        if (t.type !== "plus") return false;
        if (t.add.length !== s.add.length) return false;
        for (let i = 0; i < t.add.length; i++) {
            if (!equal(s.add[i], t.add[i])) return false;
        }
        return true;
    } else {
        if (t.type !== "psi") return false;
        return equal(s.sub, t.sub) && equal(s.arg, t.arg);
    }
}

export function psi(sub: T, arg: T): PT {
    return { type: "psi", sub: sub, arg: arg };
}

// a+b を適切に整形して返す
function plus(a: T, b: T): T {
    if (a.type === "zero") {
        return b;
    } else if (a.type === "plus") {
        if (b.type === "zero") {
            return a;
        } else if (b.type === "plus") {
            return { type: "plus", add: a.add.concat(b.add) };
        } else {
            return { type: "plus", add: [...a.add, b] };
        }
    } else {
        if (b.type === "zero") {
            return a;
        } else if (b.type === "plus") {
            return { type: "plus", add: [a, ...b.add] };
        } else {
            return { type: "plus", add: [a, b] };
        }
    }
}

// 要素が1個の配列は潰してから返す
export function sanitize_plus_term(add: PT[]): PT | AT {
    if (add.length === 1) {
        return add[0];
    } else {
        return { type: "plus", add: add };
    }
}

// s < t を判定
export function less_than(s: T, t: T): boolean {
    if (s.type === "zero") {
        return t.type !== "zero";
    } else if (s.type === "psi") {
        if (t.type === "zero") {
            return false;
        } else if (t.type === "psi") {
            return less_than(s.sub, t.sub) ||
                (equal(s.sub, t.sub) && less_than(s.arg, t.arg))
        } else {
            return equal(s, t.add[0]) || less_than(s, t.add[0]);
        }
    } else {
        if (t.type === "zero") {
            return false;
        } else if (t.type === "psi") {
            return less_than(s.add[0], t)
        } else {
            const s2 = sanitize_plus_term(s.add.slice(1));
            const t2 = sanitize_plus_term(t.add.slice(1));
            return less_than(s.add[0], t.add[0]) ||
                (equal(s.add[0], t.add[0]) && less_than(s2, t2));
        }
    }
}

// dom(t)
export function dom(s: T): ZT | PT {
    if (s.type === "zero") {
        return Z;
    } else if (s.type === "plus") {
        return dom(s.add[s.add.length - 1]);
    } else { // t.type === "psi"
        const b = s.arg;
        const domb = dom(b);
        if (domb.type === "zero") {
            const a = s.sub;
            if (a.type === "zero") {
                return s;
            } else if (a.type === "psi") {
                if (equal(a, ONE)) return s;
                if (equal(a, OMEGA)) return OMEGA;
                throw Error("未定義");
            } else {
                if (a.add.every(x => equal(x, ONE))) return s;
                throw Error("未定義");
            }
        } else if (equal(domb, ONE)) {
            return OMEGA;
        } else if (equal(domb, OMEGA)) {
            return OMEGA;
        } else {
            const d = domb.arg;
            if (d.type === "zero") {
                if (less_than(domb, s)) return domb;
                return s;
            }
            return OMEGA;
        }
    }
}

// find(s, t)
function find(s: T, t: T): T {
    if (s.type === "zero") {
        return Z;
    } else if (s.type === "plus") {
        const sub = s.add[0].sub;
        const remnant = sanitize_plus_term(s.add.slice(1));
        if (equal(sub, t)) return s;
        return find(remnant, t);
    } else {
        return s;
    }
}

// replace(s, t)
function replace(s: T, t: T): T {
    if (s.type === "zero") {
        return Z;
    } else if (s.type === "plus") {
        const a = s.add[0];
        const b = sanitize_plus_term(s.add.slice(1));
        return plus(replace(a, t), replace(b, t));
    } else {
        return psi(t, s.arg);
    }
}

// x[y]
export function fund(s: T, t: T): T {
    if (s.type === "zero") {
        return Z;
    } else if (s.type === "plus") {
        const lastfund = fund(s.add[s.add.length - 1], t);
        const remains = sanitize_plus_term(s.add.slice(0, s.add.length - 1));
        return plus(remains, lastfund);
    } else {
        const a = s.sub;
        const b = s.arg;
        const domb = dom(b);
        if (domb.type === "zero") {
            const a = s.sub;
            if (a.type === "zero") {
                return Z;
            } else if (a.type === "psi") {
                if (equal(a, ONE)) return t;
                if (equal(a, OMEGA)) return psi(fund(a, t), b);
                throw Error("未定義");
            } else {
                if (a.add.every(x => equal(x, ONE))) return t;
                throw Error("未定義");
            }
        } else if (equal(domb, ONE)) {
            if (less_than(t, OMEGA) && equal(dom(t), ONE)) {
                return plus(fund(s, fund(t, Z)), psi(a, fund(b, Z)));
            } else {
                return Z;
            }
        } else if (equal(domb, OMEGA)) {
            return psi(a, fund(b, t));
        } else {
            const d = domb.arg;
            const domd = dom(d);
            if (domd.type === "zero") {
                return psi(a, fund(b, t));
            } else {
                const e = domd.sub;
                if (b.type === "plus") {
                    const g = b.add[b.add.length - 1].sub;
                    if (less_than(t, OMEGA) && equal(dom(t), ONE)) {
                        const p = fund(s, fund(t, Z));
                        if (p.type !== "psi") throw Error("bの型がplusのときのpの型がpsiではない");
                        const gamma = p.arg;
                        return psi(a, fund(b, replace(find(gamma, g), fund(e, Z))));
                    } else {
                        return psi(a, fund(b, Z));
                    }
                } else {
                    if (less_than(t, OMEGA) && equal(dom(t), ONE)) {
                        const p = fund(s, fund(t, Z));
                        if (p.type !== "psi") throw Error("bの型がpsiのときのpの型がpsiではない");
                        const gamma = p.arg;
                        return psi(a, fund(b, replace(gamma, fund(e, Z))));
                    } else {
                        return psi(a, fund(b, Z));
                    }
                }
            }
        }
    }
}

// ===========================================
// オブジェクトから文字列へ
export function term_to_string(t: T, options: Options): string {
    if (t.type === "zero") {
        return "0";
    } else if (t.type === "psi") {
        if (!(options.checkOnOffC && t.sub.type === "zero")) {
            if (options.checkOnOffA) {
                if (options.checkOnOffB || options.checkOnOffT)
                    return headname + "_{" + term_to_string(t.sub, options) + "}(" + term_to_string(t.arg, options) + ")";
                if (t.sub.type === "zero") {
                    return headname + "_0(" + term_to_string(t.arg, options) + ")";
                } else if (t.sub.type === "plus") {
                    if (t.sub.add.every((x) => equal(x, ONE)))
                        return headname + "_" + term_to_string(t.sub, options) + "(" + term_to_string(t.arg, options) + ")";
                    return headname + "_{" + term_to_string(t.sub, options) + "}(" + term_to_string(t.arg, options) + ")";
                } else {
                    if (equal(t.sub, ONE) || (options.checkOnOffo && equal(t.sub, OMEGA)) || (options.checkOnOffO && equal(t.sub, LOMEGA)))
                        return headname + "_" + term_to_string(t.sub, options) + "(" + term_to_string(t.arg, options) + ")";
                    return headname + "_{" + term_to_string(t.sub, options) + "}(" + term_to_string(t.arg, options) + ")";
                }
            }
            return headname + "(" + term_to_string(t.sub, options) + "," + term_to_string(t.arg, options) + ")";
        }
        return headname + "(" + term_to_string(t.arg, options) + ")";
    } else {
        return t.add.map((x) => term_to_string(x, options)).join("+");
    }
}

export function abbrviate(str: string, options: Options): string {
    str = str.replace(RegExp(headname + "\\(0\\)", "g"), "1");
    str = str.replace(RegExp(headname + "_\\{0\\}\\(0\\)", "g"), "1");
    str = str.replace(RegExp(headname + "_0\\(0\\)", "g"), "1");
    str = str.replace(RegExp(headname + "\\(0,0\\)", "g"), "1");
    if (options.checkOnOffo) {
        str = str.replace(RegExp(headname + "\\(1\\)", "g"), "ω");
        str = str.replace(RegExp(headname + "_\\{0\\}\\(1\\)", "g"), "ω");
        str = str.replace(RegExp(headname + "_0\\(1\\)", "g"), "ω");
        str = str.replace(RegExp(headname + "\\(0,1\\)", "g"), "ω");
    }
    if (options.checkOnOffO) {
        str = str.replace(RegExp(headname + "_\\{1\\}\\(0\\)", "g"), "Ω");
        str = str.replace(RegExp(headname + "_1\\(0\\)", "g"), "Ω");
        str = str.replace(RegExp(headname + "\\(1,0\\)", "g"), "Ω");
    }
    if (options.checkOnOffT) str = to_TeX(str);
    while (true) {
        const numterm = str.match(/1(\+1)+/);
        if (!numterm) break;
        const matches = numterm[0].match(/1/g);
        if (!matches) throw Error("そんなことある？");
        const count = matches.length;
        str = str.replace(numterm[0], count.toString());
    }
    return str;
}

function to_TeX(str: string): string {
    str = str.replace(RegExp(headname, "g"), "\\textrm{" + headname + "}");
    str = str.replace(/ω/g, "\\omega");
    str = str.replace(/Ω/g, "\\Omega");
    return str;
}