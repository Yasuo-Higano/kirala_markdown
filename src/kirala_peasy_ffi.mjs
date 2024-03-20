import fs from "fs";

/*
import * as readline_lib from "readline";


const rl = readline_lib.createInterface({
    input: process.stdin,
    output: process.stdout
});

export function readline_async(p,callback) {
    let answer = null
    rl.question(p, (a) => {
        callback(a);
       // rl.close();
    });
}
*/

import * as readline_sync from "readline-sync";

export function readline(prompt) {
    return readline_sync.question(prompt);
}

export function show(val) {
    if( val === null ) {
        return "null";
    }
    else if( val === undefined ) {
        return "undefined";
    }
    else {
        return val.toString();
    }
}

let _outputs = "";

export function println(val) {
    _outputs += val;
    console.log(_outputs);
    _outputs = "";
}

export function print(val) {
    _outputs += val;
}

export function read_file(filepath) {
    let raw = fs.readFileSync(filepath);
    return raw;
}

export function read_text_file(filepath) {
    let text = fs.readFileSync(filepath, "utf-8");
    return text;
}

export function http_get_text(url, headers) {
}

export function now() {
    return Date.now();
}


export function get_env(key,default_value) {
    let val = process.env[key];
    if( val === undefined || val == null ) {
        return default_value;
    }
    else {
        return val;
    }
}

export function get_env_int(key,default_value) {
    let val = process.env[key];
    if( val === undefined || val == null ) {
        return default_value;
    }
    else {
        return parseInt(val);
    }
}
