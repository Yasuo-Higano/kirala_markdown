import {List} from "../prelude.mjs";

export function text(strs) {
    return strs.toArray().join("");
}

export function str(val) {
    return val.toString();
}