!function(a){var b=this;"object"==typeof exports?module.exports=a(b):"function"==typeof define&&define.amd?define([],function(){return a(b)}):b.jDataView=a(b)}(function(a){"use strict";function b(a,b){return!b&&a instanceof Array?a:Array.prototype.slice.call(a)}function c(a,b){return void 0!==a?a:b}function d(a,b,e,f){if(a instanceof d){var g=a.slice(b,b+e);return g._littleEndian=c(f,g._littleEndian),g}if(!(this instanceof d))return new d(a,b,e,f);if(this.buffer=a=d.wrapBuffer(a),this._isArrayBuffer=i.ArrayBuffer&&a instanceof ArrayBuffer,this._isPixelData=!0&&i.PixelData&&a instanceof CanvasPixelArray,this._isDataView=i.DataView&&this._isArrayBuffer,this._isNodeBuffer=!1,!(this._isArrayBuffer||this._isPixelData||a instanceof Array))throw new TypeError("jDataView buffer has an incompatible type");this._littleEndian=!!f;var h="byteLength"in a?a.byteLength:a.length;this.byteOffset=b=c(b,0),this.byteLength=e=c(e,h-b),this._offset=this._bitOffset=0,this._isDataView?this._view=new DataView(a,b,e):this._checkBounds(b,e,h),this._engineAction=this._isDataView?this._dataViewAction:this._isArrayBuffer?this._arrayBufferAction:this._arrayAction}function e(a){for(var b=i.ArrayBuffer?Uint8Array:Array,c=new b(a.length),d=0,e=a.length;e>d;d++)c[d]=255&a.charCodeAt(d);return c}function f(a){return a>=0&&31>a?1<<a:f[a]||(f[a]=Math.pow(2,a))}function g(a,b){this.lo=a,this.hi=b}function h(){g.apply(this,arguments)}var i={NodeBuffer:!1,DataView:"DataView"in a,ArrayBuffer:"ArrayBuffer"in a,PixelData:!0&&"CanvasPixelArray"in a&&"ImageData"in a&&"document"in a},j=a.TextEncoder,k=a.TextDecoder;if(i.PixelData){var l=function(a,b){var c=l.context2d.createImageData((a+3)/4,1).data;if(c.byteLength=a,void 0!==b)for(var d=0;a>d;d++)c[d]=b[d];return c};l.context2d=document.createElement("canvas").getContext("2d")}var m={Int8:1,Int16:2,Int32:4,Uint8:1,Uint16:2,Uint32:4,Float32:4,Float64:8};d.wrapBuffer=function(a){switch(typeof a){case"number":if(i.ArrayBuffer)a=new Uint8Array(a).buffer;else if(i.PixelData)a=l(a);else{a=new Array(a);for(var c=0;c<a.length;c++)a[c]=0}return a;case"string":a=e(a);default:return"length"in a&&!(i.ArrayBuffer&&a instanceof ArrayBuffer||i.PixelData&&a instanceof CanvasPixelArray)&&(i.ArrayBuffer?a instanceof ArrayBuffer||(a=new Uint8Array(a).buffer,a instanceof ArrayBuffer||(a=new Uint8Array(b(a,!0)).buffer)):a=i.PixelData?l(a.length,a):b(a)),a}},d.createBuffer=function(){return d.wrapBuffer(arguments)},d.Uint64=g,g.prototype={valueOf:function(){return this.lo+f(32)*this.hi},toString:function(){return Number.prototype.toString.apply(this.valueOf(),arguments)}},g.fromNumber=function(a){var b=Math.floor(a/f(32)),c=a-b*f(32);return new g(c,b)},d.Int64=h,h.prototype="create"in Object?Object.create(g.prototype):new g,h.prototype.valueOf=function(){return this.hi<f(31)?g.prototype.valueOf.apply(this,arguments):-(f(32)-this.lo+f(32)*(f(32)-1-this.hi))},h.fromNumber=function(a){var b,c;if(a>=0){var d=g.fromNumber(a);b=d.lo,c=d.hi}else c=Math.floor(a/f(32)),b=a-c*f(32),c+=f(32);return new h(b,c)};var n=d.prototype={compatibility:i,_checkBounds:function(a,b,d){if("number"!=typeof a)throw new TypeError("Offset is not a number.");if("number"!=typeof b)throw new TypeError("Size is not a number.");if(0>b)throw new RangeError("Length is negative.");if(0>a||a+b>c(d,this.byteLength))throw new RangeError("Offsets are out of bounds.")},_action:function(a,b,d,e,f){return this._engineAction(a,b,c(d,this._offset),c(e,this._littleEndian),f)},_dataViewAction:function(a,b,c,d,e){return this._offset=c+m[a],b?this._view["get"+a](c,d):this._view["set"+a](c,e,d)},_arrayBufferAction:function(b,d,e,f,g){var h,i=m[b],j=a[b+"Array"];if(f=c(f,this._littleEndian),1===i||(this.byteOffset+e)%i===0&&f)return h=new j(this.buffer,this.byteOffset+e,1),this._offset=e+i,d?h[0]:h[0]=g;var k=new Uint8Array(d?this.getBytes(i,e,f,!0):i);return h=new j(k.buffer,0,1),d?h[0]:(h[0]=g,void this._setBytes(e,k,f))},_arrayAction:function(a,b,c,d,e){return b?this["_get"+a](c,d):this["_set"+a](c,e,d)},_getBytes:function(a,d,e){e=c(e,this._littleEndian),d=c(d,this._offset),a=c(a,this.byteLength-d),this._checkBounds(d,a),d+=this.byteOffset,this._offset=d-this.byteOffset+a;var f=this._isArrayBuffer?new Uint8Array(this.buffer,d,a):(this.buffer.slice||Array.prototype.slice).call(this.buffer,d,d+a);return e||1>=a?f:b(f).reverse()},getBytes:function(a,d,e,f){var g=this._getBytes(a,d,c(e,!0));return f?b(g):g},_setBytes:function(a,d,e){var f=d.length;if(0!==f){if(e=c(e,this._littleEndian),a=c(a,this._offset),this._checkBounds(a,f),!e&&f>1&&(d=b(d,!0).reverse()),a+=this.byteOffset,this._isArrayBuffer)new Uint8Array(this.buffer,a,f).set(d);else for(var g=0;f>g;g++)this.buffer[a+g]=d[g];this._offset=a-this.byteOffset+f}},setBytes:function(a,b,d){this._setBytes(a,b,c(d,!0))},getString:function(a,b,c){var d=this._getBytes(a,b,!0);if(c="utf8"===c?"utf-8":c||"binary",k&&"binary"!==c)return new k(c).decode(this._isArrayBuffer?d:new Uint8Array(d));var e="";a=d.length;for(var f=0;a>f;f++)e+=String.fromCharCode(d[f]);return"utf-8"===c&&(e=decodeURIComponent(escape(e))),e},setString:function(a,b,c){c="utf8"===c?"utf-8":c||"binary";var d;j&&"binary"!==c?d=new j(c).encode(b):("utf-8"===c&&(b=unescape(encodeURIComponent(b))),d=e(b)),this._setBytes(a,d,!0)},getChar:function(a){return this.getString(1,a)},setChar:function(a,b){this.setString(a,b)},tell:function(){return this._offset},seek:function(a){return this._checkBounds(a,0),this._offset=a},skip:function(a){return this.seek(this._offset+a)},slice:function(a,b,e){function f(a,b){return 0>a?a+b:a}return a=f(a,this.byteLength),b=f(c(b,this.byteLength),this.byteLength),e?new d(this.getBytes(b-a,a,!0,!0),void 0,void 0,this._littleEndian):new d(this.buffer,this.byteOffset+a,b-a,this._littleEndian)},alignBy:function(a){return this._bitOffset=0,1!==c(a,1)?this.skip(a-(this._offset%a||a)):this._offset},_getFloat64:function(a,b){var c=this._getBytes(8,a,b),d=1-2*(c[7]>>7),e=((c[7]<<1&255)<<3|c[6]>>4)-1023,g=(15&c[6])*f(48)+c[5]*f(40)+c[4]*f(32)+c[3]*f(24)+c[2]*f(16)+c[1]*f(8)+c[0];return 1024===e?0!==g?0/0:1/0*d:-1023===e?d*g*f(-1074):d*(1+g*f(-52))*f(e)},_getFloat32:function(a,b){var c=this._getBytes(4,a,b),d=1-2*(c[3]>>7),e=(c[3]<<1&255|c[2]>>7)-127,g=(127&c[2])<<16|c[1]<<8|c[0];return 128===e?0!==g?0/0:1/0*d:-127===e?d*g*f(-149):d*(1+g*f(-23))*f(e)},_get64:function(a,b,d){d=c(d,this._littleEndian),b=c(b,this._offset);for(var e=d?[0,4]:[4,0],f=0;2>f;f++)e[f]=this.getUint32(b+e[f],d);return this._offset=b+8,new a(e[0],e[1])},getInt64:function(a,b){return this._get64(h,a,b)},getUint64:function(a,b){return this._get64(g,a,b)},_getInt32:function(a,b){var c=this._getBytes(4,a,b);return c[3]<<24|c[2]<<16|c[1]<<8|c[0]},_getUint32:function(a,b){return this._getInt32(a,b)>>>0},_getInt16:function(a,b){return this._getUint16(a,b)<<16>>16},_getUint16:function(a,b){var c=this._getBytes(2,a,b);return c[1]<<8|c[0]},_getInt8:function(a){return this._getUint8(a)<<24>>24},_getUint8:function(a){return this._getBytes(1,a)[0]},_getBitRangeData:function(a,b){var d=(c(b,this._offset)<<3)+this._bitOffset,e=d+a,f=d>>>3,g=e+7>>>3,h=this._getBytes(g-f,f,!0),i=0;(this._bitOffset=7&e)&&(this._bitOffset-=8);for(var j=0,k=h.length;k>j;j++)i=i<<8|h[j];return{start:f,bytes:h,wideValue:i}},getSigned:function(a,b){var c=32-a;return this.getUnsigned(a,b)<<c>>c},getUnsigned:function(a,b){var c=this._getBitRangeData(a,b).wideValue>>>-this._bitOffset;return 32>a?c&~(-1<<a):c},_setBinaryFloat:function(a,b,c,d,e){var g,h,i=0>b?1:0,j=~(-1<<d-1),k=1-j;0>b&&(b=-b),0===b?(g=0,h=0):isNaN(b)?(g=2*j+1,h=1):1/0===b?(g=2*j+1,h=0):(g=Math.floor(Math.log(b)/Math.LN2),g>=k&&j>=g?(h=Math.floor((b*f(-g)-1)*f(c)),g+=j):(h=Math.floor(b/f(k-c)),g=0));for(var l=[];c>=8;)l.push(h%256),h=Math.floor(h/256),c-=8;for(g=g<<c|h,d+=c;d>=8;)l.push(255&g),g>>>=8,d-=8;l.push(i<<d|g),this._setBytes(a,l,e)},_setFloat32:function(a,b,c){this._setBinaryFloat(a,b,23,8,c)},_setFloat64:function(a,b,c){this._setBinaryFloat(a,b,52,11,c)},_set64:function(a,b,d,e){d instanceof a||(d=a.fromNumber(d)),e=c(e,this._littleEndian),b=c(b,this._offset);var f=e?{lo:0,hi:4}:{lo:4,hi:0};for(var g in f)this.setUint32(b+f[g],d[g],e);this._offset=b+8},setInt64:function(a,b,c){this._set64(h,a,b,c)},setUint64:function(a,b,c){this._set64(g,a,b,c)},_setUint32:function(a,b,c){this._setBytes(a,[255&b,b>>>8&255,b>>>16&255,b>>>24],c)},_setUint16:function(a,b,c){this._setBytes(a,[255&b,b>>>8&255],c)},_setUint8:function(a,b){this._setBytes(a,[255&b])},setUnsigned:function(a,b,c){var d=this._getBitRangeData(c,a),e=d.wideValue,f=d.bytes;e&=~(~(-1<<c)<<-this._bitOffset),e|=(32>c?b&~(-1<<c):b)<<-this._bitOffset;for(var g=f.length-1;g>=0;g--)f[g]=255&e,e>>>=8;this._setBytes(d.start,f,!0)}};for(var o in m)!function(a){n["get"+a]=function(b,c){return this._action(a,!0,b,c)},n["set"+a]=function(b,c,d){this._action(a,!1,b,d,c)}}(o);n._setInt32=n._setUint32,n._setInt16=n._setUint16,n._setInt8=n._setUint8,n.setSigned=n.setUnsigned;for(var p in n)"set"===p.slice(0,3)&&!function(a){n["write"+a]=function(){Array.prototype.unshift.call(arguments,void 0),this["set"+a].apply(this,arguments)}}(p.slice(3));return d});
//# sourceMappingURL=jdataview.js.map
("function"==typeof define?function(a){define("msgpack-js",a)}:"object"==typeof exports?function(a){module.exports=a()}:function(a){this.msgpack=a()})(function(){"use strict";function a(a){if(void 0===a)return"undefined";var b,c;if(a instanceof ArrayBuffer?(c="ArrayBuffer",b=new DataView(a)):a instanceof DataView&&(c="DataView",b=a),!b)return JSON.stringify(a);for(var d=[],e=0;e<a.byteLength;e++){if(e>20){d.push("...");break}var f=b.getUint8(e).toString(16);1===f.length&&(f="0"+f),d.push(f)}return"<"+c+" "+d.join(" ")+">"}function b(a,b,c){for(var d=(a.byteLength,0),e=c.length;e>d;d++){var f=c.charCodeAt(d);if(128>f)a.setUint8(b++,f>>>0&127|0);else if(2048>f)a.setUint8(b++,f>>>6&31|192),a.setUint8(b++,f>>>0&63|128);else if(65536>f)a.setUint8(b++,f>>>12&15|224),a.setUint8(b++,f>>>6&63|128),a.setUint8(b++,f>>>0&63|128);else{if(!(1114112>f))throw new Error("bad codepoint "+f);a.setUint8(b++,f>>>18&7|240),a.setUint8(b++,f>>>12&63|128),a.setUint8(b++,f>>>6&63|128),a.setUint8(b++,f>>>0&63|128)}}}function c(a,b,c){for(var d="",e=b,f=b+c;f>e;e++){var g=a.getUint8(e);if(0!==(128&g))if(192!==(224&g))if(224!==(240&g)){if(240!==(248&g))throw new Error("Invalid byte "+g.toString(16));d+=String.fromCharCode((7&g)<<18|(63&a.getUint8(++e))<<12|(63&a.getUint8(++e))<<6|(63&a.getUint8(++e))<<0)}else d+=String.fromCharCode((15&g)<<12|(63&a.getUint8(++e))<<6|(63&a.getUint8(++e))<<0);else d+=String.fromCharCode((15&g)<<6|63&a.getUint8(++e));else d+=String.fromCharCode(g)}return d}function d(a){for(var b=0,c=0,d=a.length;d>c;c++){var e=a.charCodeAt(c);if(128>e)b+=1;else if(2048>e)b+=2;else if(65536>e)b+=3;else{if(!(1114112>e))throw new Error("bad codepoint "+e);b+=4}}return b}function e(a,b){this.offset=b||0,this.view=a}function f(a){var b=new jDataView(a),c=new e(b),d=c.parse();if(c.offset!==a.byteLength)throw new Error(a.byteLength-c.offset+" trailing bytes");return d}function g(a,c,e){var f,h,i,j,k=typeof a;if("string"===k){if(f=d(a),32>f)return c.setUint8(e,160|f),b(c,e+1,a),1+f;if(256>f)return c.setUint8(e,217),c.setUint16(e+1,f),b(c,e+2,a),2+f;if(65536>f)return c.setUint8(e,218),c.setUint16(e+1,f),b(c,e+3,a),3+f;if(4294967296>f)return c.setUint8(e,219),c.setUint32(e+1,f),b(c,e+5,a),5+f}if(a instanceof ArrayBuffer){if(f=a.byteLength,256>f)return c.setUint8(e,196),c.setUint16(e+1,f),new Uint8Array(c.buffer).set(new Uint8Array(a),e+2),2+f;if(65536>f)return c.setUint8(e,216),c.setUint16(e+1,f),new Uint8Array(c.buffer).set(new Uint8Array(a),e+3),3+f;if(4294967296>f)return c.setUint8(e,217),c.setUint32(e+1,f),new Uint8Array(c.buffer).set(new Uint8Array(a),e+5),5+f}if("number"===k){if(a%1!==0)return c.setUint8(e,203),c.setFloat64(e+1,a),9;if(a>=0){if(128>a)return c.setUint8(e,a),1;if(256>a)return c.setUint8(e,204),c.setUint8(e+1,a),2;if(65536>a)return c.setUint8(e,205),c.setUint16(e+1,a),3;if(4294967296>a)return c.setUint8(e,206),c.setUint32(e+1,a),5;if(0x10000000000000000>a)return c.setUint8(e,207),c.setUint64(e+1,a),9;throw new Error("Number too big 0x"+a.toString(16))}if(a>=-32)return c.setInt8(e,a),1;if(a>=-128)return c.setUint8(e,208),c.setInt8(e+1,a),2;if(a>=-32768)return c.setUint8(e,209),c.setInt16(e+1,a),3;if(a>=-2147483648)return c.setUint8(e,210),c.setInt32(e+1,a),5;if(a>=-2147483648)return c.setUint8(e,211),c.setUint64(e+1,a),9;throw new Error("Number too small -0x"+(-a).toString(16).substr(1))}if("undefined"===k)return c.setUint8(e,212),c.setUint8(e+1,0),c.setUint8(e+2,0),1;if(null===a)return c.setUint8(e,192),1;if("boolean"===k)return c.setUint8(e,a?195:194),1;if("function"==typeof a.toJSON)return g(a.toJSON(),c,e);if("object"===k){if(h=0,i=Array.isArray(a))f=a.length;else{var l=Object.keys(a);f=l.length}if(16>f?(c.setUint8(e,f|(i?144:128)),h=1):65536>f?(c.setUint8(e,i?220:222),c.setUint16(e+1,f),h=3):4294967296>f&&(c.setUint8(e,i?221:223),c.setUint32(e+1,f),h=5),i)for(j=0;f>j;j++)h+=g(a[j],c,e+h);else for(j=0;f>j;j++){var m=l[j];h+=g(m,c,e+h),h+=g(a[m],c,e+h)}return h}throw new Error("Unknown type "+k)}function h(a){var b,c,e,f,g=typeof a;if("string"===g){if(b=d(a),32>b)return 1+b;if(256>b)return 2+b;if(65536>b)return 3+b;if(4294967296>b)return 5+b}if(a instanceof ArrayBuffer){if(b=a.byteLength,256>b)return 2+b;if(65536>b)return 3+b;if(4294967296>b)return 5+b}if("number"===g){if(a<<0!==a)return 9;if(a>=0){if(128>a)return 1;if(256>a)return 2;if(65536>a)return 3;if(4294967296>a)return 5;if(0x10000000000000000>a)return 9;throw new Error("Number too big 0x"+a.toString(16))}if(a>=-32)return 1;if(a>=-128)return 2;if(a>=-32768)return 3;if(a>=-2147483648)return 5;if(a>=-0x8000000000000000)return 9;throw new Error("Number too small -0x"+a.toString(16).substr(1))}if("boolean"===g||null===a)return 1;if("undefined"===g)return 3;if("function"==typeof a.toJSON)return h(a.toJSON());if("object"===g){if("function"==typeof a.toJSON&&(a=a.toJSON()),c=0,Array.isArray(a))for(b=a.length,f=0;b>f;f++)c+=h(a[f]);else for(e=Object.keys(a),b=e.length,f=0;b>f;f++)c+=h(e[f])+h(a[e[f]]);if(16>b)return 1+c;if(65536>b)return 3+c;if(4294967296>b)return 5+c;throw new Error("Array or object too long 0x"+b.toString(16))}if("function"===g)return 0;throw new Error("Unknown type "+g)}var i={};return i.inspect=a,i.utf8Write=b,i.utf8Read=c,i.utf8ByteCount=d,i.encode=function(a){var b=new ArrayBuffer(h(a)),c=new jDataView(b);return g(a,c,0),b},i.decode=f,e.prototype.map=function(a){for(var b={},c=0;a>c;c++){var d=this.parse();b[d]=this.parse()}return b},e.prototype.buf=function(a){var b=new ArrayBuffer(a);return new Uint8Array(b).set(new Uint8Array(this.view.buffer,this.offset,a),0),this.offset+=a,b},e.prototype.u8str=function(a){var b=c(this.view,this.offset,a);return this.offset+=a,b},e.prototype.array=function(a){for(var b=new Array(a),c=0;a>c;c++)b[c]=this.parse();return b},e.prototype.parse=function(){var a,b,c,d=this.view.getUint8(this.offset);if(0===(128&d))return this.offset++,d;if(128===(240&d))return b=15&d,this.offset++,this.map(b);if(144===(240&d))return b=15&d,this.offset++,this.array(b);if(160===(224&d))return b=31&d,this.offset++,this.u8str(b);if(224===(224&d))return a=this.view.getInt8(this.offset),this.offset++,a;switch(d){case 192:return this.offset++,null;case 194:return this.offset++,!1;case 195:return this.offset++,!0;case 196:return b=this.view.getUint8(this.offset+1),this.offset+=2,this.buf(b);case 197:return b=this.view.getUint16(this.offset+1),this.offset+=3,this.buf(b);case 198:return b=this.view.getUint32(this.offset+1),this.offset+=5,this.buf(b);case 199:return b=this.view.getUint8(this.offset+1),c=this.view.getUint8(this.offset+2),this.offset+=3,[c,this.buf(b)];case 200:return b=this.view.getUint16(this.offset+1),c=this.view.getUint8(this.offset+3),this.offset+=4,[c,this.buf(b)];case 201:return b=this.view.getUint32(this.offset+1),c=this.view.getUint8(this.offset+5),this.offset+=6,[c,this.buf(b)];case 202:return a=this.view.getFloat32(this.offset+1),this.offset+=5,a;case 203:return a=this.view.getFloat64(this.offset+1),this.offset+=9,a;case 204:return a=this.view.getUint8(this.offset+1),this.offset+=2,a;case 205:return a=this.view.getUint16(this.offset+1),this.offset+=3,a;case 206:return a=this.view.getUint32(this.offset+1),this.offset+=5,a;case 207:return a=this.view.getUint64(this.offset+1),this.offset+=9,parseInt(a);case 208:return a=this.view.getInt8(this.offset+1),this.offset+=2,a;case 209:return a=this.view.getInt16(this.offset+1),this.offset+=3,a;case 210:return a=this.view.getInt32(this.offset+1),this.offset+=5,a;case 211:return a=this.view.getInt64(this.offset+1),this.offset+=9,parseInt(a);case 212:return c=this.view.getUint8(this.offset+1),a=this.view.getUint8(this.offset+2),this.offset+=3,0===c&&0===a?void 0:[c,a];case 213:return c=this.view.getUint8(this.offset+1),this.offset+=2,[c,this.buf(2)];case 214:return c=this.view.getUint8(this.offset+1),this.offset+=2,[c,this.buf(4)];case 215:return c=this.view.getUint8(this.offset+1),this.offset+=2,[c,this.buf(8)];case 216:return c=this.view.getUint8(this.offset+1),this.offset+=2,[c,this.buf(16)];case 217:return b=this.view.getUint8(this.offset+1),this.offset+=2,this.u8str(b);case 218:return b=this.view.getUint16(this.offset+1),this.offset+=3,this.u8str(b);case 219:return b=this.view.getUint32(this.offset+1),this.offset+=5,this.u8str(b);case 220:return b=this.view.getUint16(this.offset+1),this.offset+=3,this.array(b);case 221:return b=this.view.getUint32(this.offset+1),this.offset+=5,this.array(b);case 222:return b=this.view.getUint16(this.offset+1),this.offset+=3,this.map(b);case 223:return b=this.view.getUint32(this.offset+1),this.offset+=5,this.map(b)}throw new Error("Unknown type 0x"+d.toString(16))},i});
//# sourceMappingURL=msgpack.min.js.map


/**
 * Project: wampy.js
 *
 * https://github.com/KSDaemon/wampy.js
 *
 * A lightweight client-side implementation of
 * WAMP (The WebSocket Application Messaging Protocol v2)
 * http://wamp.ws
 *
 * Provides asynchronous RPC/PubSub over WebSocket.
 *
 * Copyright 2014 KSDaemon. Licensed under the MIT License.
 * See @license text at http://www.opensource.org/licenses/mit-license.php
 *
 */

// Module boilerplate to support browser globals and browserify and AMD.
;(typeof define === 'function' ? function (m) { define('Wampy', m); } :
    typeof exports === 'object' ? function (m) { module.exports = m(); } :
    function (m) { this.Wampy = m(); }
)(function () {

    var WAMP_MSG_SPEC = {
        HELLO: 1,
        WELCOME: 2,
        ABORT: 3,
        CHALLENGE: 4,
        AUTHENTICATE: 5,
        GOODBYE: 6,
        HEARTBEAT: 7,
        ERROR: 8,
        PUBLISH: 16,
        PUBLISHED: 17,
        SUBSCRIBE: 32,
        SUBSCRIBED: 33,
        UNSUBSCRIBE: 34,
        UNSUBSCRIBED: 35,
        EVENT: 36,
        CALL: 48,
        CANCEL: 49,
        RESULT: 50,
        REGISTER: 64,
        REGISTERED: 65,
        UNREGISTER: 66,
        UNREGISTERED: 67,
        INVOCATION: 68,
        INTERRUPT: 69,
        YIELD: 70
    },

    WAMP_ERROR_MSG = {
        SUCCESS: {
            code: 0,
            description: 'Success!'
        },
        URI_ERROR: {
            code: 1,
            description: 'Topic URI doesn\'t meet requirements!'
        },
        NO_BROKER: {
            code: 2,
            description: 'Server doesn\'t provide broker role!'
        },
        NO_CALLBACK_SPEC: {
            code: 3,
            description: 'No required callback function specified!'
        },
        INVALID_PARAM: {
            code: 4,
            description: 'Invalid parameter(s) specified!'
        },
        NON_EXIST_SUBSCRIBE_CONFIRM: {
            code: 5,
            description: 'Received subscribe confirmation to non existent subscription!'
        },
        NON_EXIST_SUBSCRIBE_ERROR: {
            code: 6,
            description: 'Received error for non existent subscription!'
        },
        NON_EXIST_UNSUBSCRIBE: {
            code: 7,
            description: 'Trying to unsubscribe from non existent subscription!'
        },
        NON_EXIST_SUBSCRIBE_UNSUBSCRIBED: {
            code: 8,
            description: 'Received unsubscribe confirmation to non existent subscription!'
        },
        NON_EXIST_PUBLISH_ERROR: {
            code: 9,
            description: 'Received error for non existent publication!'
        },
        NON_EXIST_PUBLISH_PUBLISHED: {
            code: 10,
            description: 'Received publish confirmation for non existent publication!'
        },
        NON_EXIST_SUBSCRIBE_EVENT: {
            code: 11,
            description: 'Received event for non existent subscription!'
        },
        NO_DEALER: {
            code: 12,
            description: 'Server doesn\'t provide dealer role!'
        },
        NON_EXIST_CALL_RESULT: {
            code: 13,
            description: 'Received rpc result for non existent call!'
        },
        NON_EXIST_CALL_ERROR: {
            code: 14,
            description: 'Received rpc call error for non existent call!'
        },
        RPC_ALREADY_REGISTERED: {
            code: 15,
            description: 'RPC already registered!'
        },
        NON_EXIST_RPC_REG: {
            code: 16,
            description: 'Received rpc registration confirmation for non existent rpc!'
        },
        NON_EXIST_RPC_UNREG: {
            code: 17,
            description: 'Received rpc unregistration confirmation for non existent rpc!'
        },
        NON_EXIST_RPC_ERROR: {
            code: 18,
            description: 'Received error for non existent rpc!'
        },
        NON_EXIST_RPC_INVOCATION: {
            code: 19,
            description: 'Received invocation for non existent rpc!'
        },
        NON_EXIST_RPC_REQ_ID: {
            code: 20,
            description: 'No RPC calls in action with specified request ID!'
        },
        NO_REALM: {
            code: 21,
            description: 'No realm specified!'
        },
        NO_WS_URL: {
            code: 22,
            description: 'No websocket URL specified or URL is incorrect!'
        }
    },

    isNode = (typeof process === 'object' && Object.prototype.toString.call(process) === '[object process]');

    function getServerUrlBrowser(url) {
        var scheme, port;

        if (!url) {
            scheme = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
            port = window.location.port !== '' ? ':' + window.location.port : '';
            return scheme + window.location.hostname + port + '/ws';
        } else if (/^ws(s)?:\/\//.test(url)) {   // ws scheme is specified
            return url;
        } else if (/:\d{1,5}/.test(url)) {  // no scheme, but port is specified
            scheme = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
            return scheme + url;
        } else if (url[0] === '/') {    // just path on current server
            scheme = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
            port = window.location.port !== '' ? ':' + window.location.port : '';
            return scheme + window.location.hostname + port + url;
        } else {    // domain
            scheme = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
            return scheme + url;
        }
    }

    function getServerUrlNode(url) {
        if (/^ws(s)?:\/\//.test(url)) {   // ws scheme is specified
            return url;
        } else {
            return null;
        }
    }

    function getWebSocket(url, protocols) {
        var parsedUrl = isNode ? getServerUrlNode(url) : getServerUrlBrowser(url),
            root = isNode ? global : window;

        if (!parsedUrl) {
            return null;
        }

        if ('WebSocket' in root) {
        // Chrome, MSIE, newer Firefox
            if (protocols) {
                return new root.WebSocket(parsedUrl, protocols);
            } else {
                return new root.WebSocket(parsedUrl);
            }
        } else if ('MozWebSocket' in root) {
            // older versions of Firefox
            if (protocols) {
                return new root.MozWebSocket(parsedUrl, protocols);
            } else {
                return new root.MozWebSocket(parsedUrl);
            }
        } else {
            return null;
        }
    }

    /**
     * WAMP Client Class
     * @param {string} url
     * @param {Object} options
     */
    var Wampy = function (url, options) {

        /**
         * Wampy version
         * @type {string}
         * @private
         */
        this.version = 'v1.0.6';

        /**
         * WS Url
         * @type {string}
         * @private
         */
        this._url = (typeof arguments[0] === 'string') ? url : undefined;

        /**
         * WS protocols
         * @type {Array}
         * @private
         */
        this._protocols = ['wamp.2.json'];

        /**
         * WAMP features, supported by Wampy
         * @type {object}
         * @private
         */
        this._wamp_features = {
            agent: 'Wampy.js ' + this.version,
            roles: {
                publisher: {
                    features: {
                        subscriber_blackwhite_listing: true,
                        publisher_exclusion: true,
                        publisher_identification: true
                    }
                },
                subscriber: {},
                caller: {
                    features: {
                        callee_blackwhite_listing: true,
                        caller_exclusion: true,
                        caller_identification: true,
                        progressive_call_results: true,
                        call_canceling: true
                    }
                },
                callee: {
                    features: {
                        caller_identification: true
                    }
                }
            }
        };

        /**
         * Internal cache for object lifetime
         * @type {Object}
         * @private
         */
        this._cache = {
            /**
             * WAMP Session ID
             * @type {string}
             */
            sessionId: null,

            /**
             * Server WAMP roles and features
             */
            server_wamp_features: { roles: {} },

            /**
             * Are we in state of saying goodbye
             * @type {boolean}
             */
            isSayingGoodbye: false,

            /**
             * Status of last operation
             */
            opStatus: { code: 0, description: 'Success!', reqId: 0 },

            /**
             * Timer for reconnection
             * @type {null}
             */
            timer: null,

            /**
             * Reconnection attempts
             * @type {number}
             */
            reconnectingAttempts: 0
        };

        /**
         * WebSocket object
         * @type {Object}
         * @private
         */
        this._ws = null;

        /**
         * Internal queue for websocket requests, for case of disconnect
         * @type {Array}
         * @private
         */
        this._wsQueue = [];

        /**
         * Internal queue for wamp requests
         * @type {object}
         * @private
         */
        this._requests = {};

        /**
         * Stored RPC
         * @type {object}
         * @private
         */
        this._calls = {};

        /**
         * Stored Pub/Sub
         * @type {object}
         * @private
         */
        this._subscriptions = {};

        /**
         * Stored Pub/Sub topics
         * @type {Array}
         * @private
         */
        this._subsTopics = [];

        /**
         * Stored RPC Registrations
         * @type {object}
         * @private
         */
        this._rpcRegs = {};

        /**
         * Stored RPC names
         * @type {Array}
         * @private
         */
        this._rpcNames = [];

        /**
         * Options hash-table
         * @type {Object}
         * @private
         */
        this._options = {
            /**
             * Logging
             * @type {boolean}
             */
            debug: false,

            /**
             * Reconnecting flag
             * @type {boolean}
             */
            autoReconnect: true,

            /**
             * Reconnecting interval (in ms)
             * @type {number}
             */
            reconnectInterval: 2 * 1000,

            /**
             * Maximum reconnection retries
             * @type {number}
             */
            maxRetries: 25,

            /**
             * Message serializer
             * @type {string}
             */
            transportEncoding: 'json',

            /**
             * WAMP Realm to join
             * @type {string}
             */
            realm: null,

            /**
             * onConnect callback
             * @type {function}
             */
            onConnect: null,

            /**
             * onClose callback
             * @type {function}
             */
            onClose: null,

            /**
             * onError callback
             * @type {function}
             */
            onError: null,

            /**
             * onReconnect callback
             * @type {function}
             */
            onReconnect: null

        };

        switch (arguments.length) {
            case 1:
                if (typeof arguments[0] !== 'string') {
                    this._options = this._merge(this._options, arguments[0]);
                }
                break;
            case 2:
                this._options = this._merge(this._options, options);
                break;
        }

        this.connect();
    };

    /* Internal utils methods */
    /**
     * Internal logger
     * @param obj
     * @private
     */
    Wampy.prototype._log = function () {
        if (this._options.debug) {
            console.log(arguments);
        }
    };

    /**
     * Get the new unique request id
     * @returns {number}
     * @private
     */
    Wampy.prototype._getReqId = function () {
        var reqId;

        do {
            /* Lua (and cjson) outputs big numbers in scientific notation :(
             * Need to find a way of fixing that
             * For now, i think it's not a big problem to reduce range.
             */
//          reqId = Math.floor(Math.random() * 9007199254740992);
            reqId = Math.floor(Math.random() * 100000000000000);
        } while (reqId in this._requests);

        return reqId;
    };

    /**
     * Merge argument objects into one
     * @returns {Object}
     * @private
     */
    Wampy.prototype._merge = function () {
        var obj = {}, i, l = arguments.length, attr;

        for (i = 0; i < l; i++) {
            for (attr in arguments[i]) {
                obj[attr] = arguments[i][attr];
            }
        }

        return obj;
    };

    /**
     * Check if value is array
     * @param obj
     * @returns {boolean}
     * @private
     */
    Wampy.prototype._isArray = function (obj) {
        return (!!obj) && (obj.constructor === Array);
    };

    /**
     * Check if value is object
     * @param obj
     * @returns {boolean}
     * @private
     */
    Wampy.prototype._isObject = function (obj) {
        return obj === Object(obj) && Object.prototype.toString.call(obj) !== '[object Array]';
    };

    /**
     * Check if value is object literal
     * @param obj
     * @returns {boolean}
     * @private
     */
    Wampy.prototype._isPlainObject = function (obj) {
        return (!!obj) && (obj.constructor === Object);
    };

    /**
     * Fix websocket protocols based on options
     * @private
     */
    Wampy.prototype._setWsProtocols = function () {
        var root = isNode ? global : window;

        if (root.msgpack !== undefined) {
            if (this._options.transportEncoding === 'msgpack') {
                this._protocols = ['wamp.2.msgpack', 'wamp.2.json'];
            } else {
                this._protocols = ['wamp.2.json', 'wamp.2.msgpack'];
            }
        }

    };

    /**
     * Validate uri
     * @param {string} uri
     * @returns {boolean}
     * @private
     */
    Wampy.prototype._validateURI = function (uri) {
        var re = /^([0-9a-zA-Z_]{2,}\.)*([0-9a-zA-Z_]{2,})$/;
        if (!re.test(uri)) {
            return false;
        } else {
            return true;
        }
    };

    /**
     * Encode WAMP message
     * @param {Array} msg
     * @returns {*}
     * @private
     */
    Wampy.prototype._encode = function (msg) {
        var bytearray;

        if (this._options.transportEncoding === 'msgpack') {
            try {
                bytearray = new Uint8Array(msgpack.encode(msg));

                return bytearray.buffer;

            } catch (e) {
                throw new Error('[wampy] no msgpack encoder available!');
            }
        } else {
            return JSON.stringify(msg);
        }
    };

    /**
     * Decode WAMP message
     * @param  msg
     * @returns {array}
     * @private
     */
    Wampy.prototype._decode = function (msg) {
        if (this._options.transportEncoding === 'msgpack') {
            try {

                return msgpack.decode(msg);

            } catch (e) {
                throw new Error('[wampy] no msgpack encoder available!');
            }
        } else {
            return JSON.parse(msg);
        }
    };

    /**
     * Send encoded message to server
     * @param {Array} msg
     * @private
     */
    Wampy.prototype._send = function (msg) {
        if (msg) {
            this._wsQueue.push(this._encode(msg));
        }

        if (this._ws && this._ws.readyState === 1 && this._cache.sessionId) {
            while (this._wsQueue.length) {
                this._ws.send(this._wsQueue.shift());
            }
        }
    };

    /**
     * Reset internal state and cache
     * @private
     */
    Wampy.prototype._resetState = function () {
        this._wsQueue = [];
        this._subscriptions = {};
        this._subsTopics = [];
        this._requests = {};
        this._calls = {};
        this._rpcRegs = {};
        this._rpcNames = [];

        // Just keep attrs that are have to be present
        this._cache = {
            reconnectingAttempts: 0
        };
    };

    /**
     * Initialize internal websocket callbacks
     * @private
     */
    Wampy.prototype._initWsCallbacks = function () {
        var self = this;

        if (this._ws) {
            this._ws.onopen = function () { self._wsOnOpen.call(self); };
            this._ws.onclose = function (event) { self._wsOnClose.call(self, event); };
            this._ws.onmessage = function (event) { self._wsOnMessage.call(self, event); };
            this._ws.onerror = function (error) { self._wsOnError.call(self, error); };
        }
    };

    Wampy.prototype._wsOnOpen = function () {
        var p;

        this._log('[wampy] websocket connected');

        p = this._ws.protocol.split('.');
        this._options.transportEncoding = p[2];

        if (this._options.transportEncoding === 'msgpack') {
            this._ws.binaryType = 'arraybuffer';
        }

        // WAMP SPEC: [HELLO, Realm|uri, Details|dict]
        // Sending directly 'cause it's a hello msg and no sessionId check is needed
        this._ws.send(this._encode([WAMP_MSG_SPEC.HELLO, this._options.realm, this._wamp_features]));
    };

    Wampy.prototype._wsOnClose = function () {
        var self = this,
            root = isNode ? global : window;
        this._log('[wampy] websocket disconnected');

        // Automatic reconnection
        if ((this._cache.sessionId || this._cache.reconnectingAttempts) &&
            this._options.autoReconnect && this._cache.reconnectingAttempts < this._options.maxRetries &&
            !this._cache.isSayingGoodbye) {
            this._cache.sessionId = null;
            this._cache.timer = root.setTimeout(function () {
                self._wsReconnect.call(self);
            }, this._options.reconnectInterval);
        } else {
            // No reconnection needed or reached max retries count
            if (this._options.onClose) {
                this._options.onClose();
            }

            this._resetState();
            this._ws = null;
        }
    };

    Wampy.prototype._wsOnMessage = function (event) {
        var data, id, i, d, result, msg;

        this._log('[wampy] websocket message received', event.data);

        data = this._decode(event.data);

        switch (data[0]) {
            case WAMP_MSG_SPEC.WELCOME:
                // WAMP SPEC: [WELCOME, Session|id, Details|dict]

                this._cache.sessionId = data[1];
                this._cache.server_wamp_features = data[2];

                // Firing onConnect event on real connection to WAMP server
                if (this._options.onConnect) {
                    this._options.onConnect();
                }

                if (this._cache.reconnectingAttempts) {
                    // There was reconnection

                    this._cache.reconnectingAttempts = 0;

                    // Let's renew all previous state
                    this._renewSubscriptions();
                    this._renewRegistrations();

                }

                // Send local queue if there is something out there
                this._send();

                break;
            case WAMP_MSG_SPEC.ABORT:
                // WAMP SPEC: [ABORT, Details|dict, Reason|uri]
                if (this._options.onError) {
                    this._options.onError(data[1].message ? data[1].message : data[2]);
                }
                this._ws.close();
                break;
            case WAMP_MSG_SPEC.CHALLENGE:

                break;
            case WAMP_MSG_SPEC.GOODBYE:
                // WAMP SPEC: [GOODBYE, Details|dict, Reason|uri]
                if (!this._cache.isSayingGoodbye) {    // get goodbye, initiated by server
                    this._cache.isSayingGoodbye = true;
                    this._send([WAMP_MSG_SPEC.GOODBYE, {}, 'wamp.error.goodbye_and_out']);
                }
                this._cache.sessionId = null;
                this._ws.close();
                break;
            case WAMP_MSG_SPEC.HEARTBEAT:

                break;
            case WAMP_MSG_SPEC.ERROR:
                // WAMP SPEC: [ERROR, REQUEST.Type|int, REQUEST.Request|id, Details|dict,
                //             Error|uri, (Arguments|list, ArgumentsKw|dict)]
                switch (data[1]) {
                    case WAMP_MSG_SPEC.SUBSCRIBE:
                    case WAMP_MSG_SPEC.UNSUBSCRIBE:
                        if (this._requests[data[2]]) {

                            if (this._requests[data[2]].callbacks.onError) {
                                this._requests[data[2]].callbacks.onError(data[4]);
                            }

                            delete this._requests[data[2]];

                        } else {
                            this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_SUBSCRIBE_ERROR;
                        }
                        break;
                    case WAMP_MSG_SPEC.PUBLISH:
                        if (this._requests[data[2]]) {

                            if (this._requests[data[2]].callbacks.onError) {
                                this._requests[data[2]].callbacks.onError(data[4]);
                            }

                            delete this._requests[data[2]];

                        } else {
                            this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_PUBLISH_ERROR;
                        }
                        break;
                    case WAMP_MSG_SPEC.REGISTER:
                    case WAMP_MSG_SPEC.UNREGISTER:
                        // WAMP SPEC: [ERROR, REGISTER, REGISTER.Request|id, Details|dict, Error|uri]
                        if (this._requests[data[2]]) {

                            if (this._requests[data[2]].callbacks.onError) {
                                this._requests[data[2]].callbacks.onError(data[4]);
                            }

                            delete this._requests[data[2]];

                        } else {
                            this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_RPC_ERROR;
                        }
                        break;
                    case WAMP_MSG_SPEC.INVOCATION:
                        break;
                    case WAMP_MSG_SPEC.CALL:
                        if (this._calls[data[2]]) {

                            if (this._calls[data[2]].onError) {

                                switch (data.length) {
                                    case 5:
                                        // WAMP SPEC: [ERROR, CALL, CALL.Request|id, Details|dict, Error|uri]
                                        d = null;
                                        break;
                                    case 6:
                                        // WAMP SPEC: [ERROR, CALL, CALL.Request|id, Details|dict,
                                        //             Error|uri, Arguments|list]
                                        d = data[5];
                                        break;
                                    case 7:
                                        // WAMP SPEC: [ERROR, CALL, CALL.Request|id, Details|dict,
                                        //             Error|uri, Arguments|list, ArgumentsKw|dict]
                                        d = data[6];
                                        break;
                                }

                                this._calls[data[2]].onError(d);
                            }

                            delete this._calls[data[2]];

                        } else {
                            this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_CALL_ERROR;
                        }
                        break;
                }
                break;
            case WAMP_MSG_SPEC.SUBSCRIBED:
                // WAMP SPEC: [SUBSCRIBED, SUBSCRIBE.Request|id, Subscription|id]
                if (this._requests[data[1]]) {
                    this._subscriptions[this._requests[data[1]].topic] = this._subscriptions[data[2]] = {
                        id: data[2],
                        callbacks: [this._requests[data[1]].callbacks.onEvent]
                    };

                    this._subsTopics.push(this._requests[data[1]].topic);

                    if (this._requests[data[1]].callbacks.onSuccess) {
                        this._requests[data[1]].callbacks.onSuccess();
                    }

                    delete this._requests[data[1]];

                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_SUBSCRIBE_CONFIRM;
                }
                break;
            case WAMP_MSG_SPEC.UNSUBSCRIBED:
                // WAMP SPEC: [UNSUBSCRIBED, UNSUBSCRIBE.Request|id]
                if (this._requests[data[1]]) {
                    id = this._subscriptions[this._requests[data[1]].topic].id;
                    delete this._subscriptions[this._requests[data[1]].topic];
                    delete this._subscriptions[id];

                    i = this._subsTopics.indexOf(this._requests[data[1]].topic);
                    if (i >= 0) {
                        this._subsTopics.splice(i, 1);
                    }

                    if (this._requests[data[1]].callbacks.onSuccess) {
                        this._requests[data[1]].callbacks.onSuccess();
                    }

                    delete this._requests[data[1]];
                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_SUBSCRIBE_UNSUBSCRIBED;
                }
                break;
            case WAMP_MSG_SPEC.PUBLISHED:
                // WAMP SPEC: [PUBLISHED, PUBLISH.Request|id, Publication|id]
                if (this._requests[data[1]]) {
                    if (this._requests[data[1]].callbacks.onSuccess) {
                        this._requests[data[1]].callbacks.onSuccess();
                    }

                    delete this._requests[data[1]];

                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_PUBLISH_PUBLISHED;
                }
                break;
            case WAMP_MSG_SPEC.EVENT:
                if (this._subscriptions[data[1]]) {

                    switch (data.length) {
                        case 4:
                            // WAMP SPEC: [EVENT, SUBSCRIBED.Subscription|id, PUBLISHED.Publication|id, Details|dict]
                            d = null;
                            break;
                        case 5:
                            // WAMP SPEC: [EVENT, SUBSCRIBED.Subscription|id, PUBLISHED.Publication|id,
                            //             Details|dict, PUBLISH.Arguments|list]
                            d = data[4];
                            break;
                        case 6:
                            // WAMP SPEC: [EVENT, SUBSCRIBED.Subscription|id, PUBLISHED.Publication|id,
                            //             Details|dict, PUBLISH.Arguments|list, PUBLISH.ArgumentKw|dict]
                            d = data[5];
                            break;
                    }

                    i = this._subscriptions[data[1]].callbacks.length;
                    while (i--) {
                        this._subscriptions[data[1]].callbacks[i](d);
                    }

                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_SUBSCRIBE_EVENT;
                }
                break;
            case WAMP_MSG_SPEC.RESULT:
                if (this._calls[data[1]]) {

                    switch (data.length) {
                        case 3:
                            // WAMP SPEC: [RESULT, CALL.Request|id, Details|dict]
                            d = null;
                            break;
                        case 4:
                            // WAMP SPEC: [RESULT, CALL.Request|id, Details|dict, YIELD.Arguments|list]
                            d = data[3];
                            break;
                        case 5:
                            // WAMP SPEC: [RESULT, CALL.Request|id, Details|dict,
                            //             YIELD.Arguments|list, YIELD.ArgumentsKw|dict]
                            d = data[4];
                            break;
                    }

                    this._calls[data[1]].onSuccess(d);
                    if (!(data[2].progress && data[2].progress === true)) {
                        // We receive final result (progressive or not)
                        delete this._calls[data[1]];
                    }

                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_CALL_RESULT;
                }
                break;
            case WAMP_MSG_SPEC.REGISTER:
                // WAMP SPEC:
                break;
            case WAMP_MSG_SPEC.REGISTERED:
                // WAMP SPEC: [REGISTERED, REGISTER.Request|id, Registration|id]
                if (this._requests[data[1]]) {
                    this._rpcRegs[this._requests[data[1]].topic] = this._rpcRegs[data[2]] = {
                        id: data[2],
                        callbacks: [this._requests[data[1]].callbacks.rpc]
                    };

                    this._rpcNames.push(this._requests[data[1]].topic);

                    if (this._requests[data[1]].callbacks.onSuccess) {
                        this._requests[data[1]].callbacks.onSuccess();
                    }

                    delete this._requests[data[1]];

                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_RPC_REG;
                }
                break;
            case WAMP_MSG_SPEC.UNREGISTER:
                // WAMP SPEC:
                break;
            case WAMP_MSG_SPEC.UNREGISTERED:
                // WAMP SPEC: [UNREGISTERED, UNREGISTER.Request|id]
                if (this._requests[data[1]]) {
                    id = this._rpcRegs[this._requests[data[1]].topic].id;
                    delete this._rpcRegs[this._requests[data[1]].topic];
                    delete this._rpcRegs[id];

                    i = this._rpcNames.indexOf(this._requests[data[1]].topic);
                    if (i >= 0) {
                        this._rpcNames.splice(i, 1);
                    }

                    if (this._requests[data[1]].callbacks.onSuccess) {
                        this._requests[data[1]].callbacks.onSuccess();
                    }

                    delete this._requests[data[1]];
                } else {
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_RPC_UNREG;
                }
                break;
            case WAMP_MSG_SPEC.INVOCATION:
                if (this._rpcRegs[data[2]]) {

                    switch (data.length) {
                        case 4:
                            // WAMP SPEC: [INVOCATION, Request|id, REGISTERED.Registration|id, Details|dict]
                            d = null;
                            break;
                        case 5:
                            // WAMP SPEC: [INVOCATION, Request|id, REGISTERED.Registration|id,
                            //             Details|dict, CALL.Arguments|list]
                            d = data[4];
                            break;
                        case 6:
                            // WAMP SPEC: [INVOCATION, Request|id, REGISTERED.Registration|id,
                            //             Details|dict, CALL.Arguments|list, CALL.ArgumentsKw|dict]
                            d = data[5];
                            break;
                    }

                    try {
                        result = this._rpcRegs[data[2]].callbacks[0](d);
                    } catch (e) {
                        this._send([WAMP_MSG_SPEC.ERROR, WAMP_MSG_SPEC.INVOCATION,
                                    data[1], {}, 'wamp.error.invocation_exception']);
                        return ;
                    }

                    // WAMP SPEC: [YIELD, INVOCATION.Request|id, Options|dict, (Arguments|list, ArgumentsKw|dict)]
                    if (this._isArray(result)) {
                        msg = [WAMP_MSG_SPEC.YIELD, data[1], {}, result];
                    } else if (this._isPlainObject(result)) {
                        msg = [WAMP_MSG_SPEC.YIELD, data[1], {}, [], result];
                    } else if (result === undefined) {
                        msg = [WAMP_MSG_SPEC.YIELD, data[1], {}];
                    } else {    // single value
                        msg = [WAMP_MSG_SPEC.YIELD, data[1], {}, [result]];
                    }

                    this._send(msg);

                } else {
                    // WAMP SPEC: [ERROR, INVOCATION, INVOCATION.Request|id, Details|dict, Error|uri]
                    this._send([WAMP_MSG_SPEC.ERROR, WAMP_MSG_SPEC.INVOCATION,
                                data[1], {}, 'wamp.error.no_such_procedure']);
                    this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_RPC_INVOCATION;
                }

                break;
            case WAMP_MSG_SPEC.INTERRUPT:
                // WAMP SPEC:
                break;
            case WAMP_MSG_SPEC.YIELD:
                // WAMP SPEC:
                break;
        }
    };

    Wampy.prototype._wsOnError = function (error) {
        this._log('[wampy] websocket error');

        if (this._options.onError) {
            this._options.onError(error);
        }
    };

    Wampy.prototype._wsReconnect = function () {
        this._log('[wampy] websocket reconnecting...');

        if (this._options.onReconnect) {
            this._options.onReconnect();
        }

        this._cache.reconnectingAttempts++;
        this._ws = getWebSocket(this._url, this._protocols);
        this._initWsCallbacks();
    };

    Wampy.prototype._renewSubscriptions = function () {
        var subs = this._subscriptions,
            st = this._subsTopics,
            s, i;

        this._subscriptions = {};
        this._subsTopics = [];

        s = st.length;
        while (s--) {
            i = subs[st[s]].callbacks.length;
            while (i--) {
                this.subscribe(st[s], subs[st[s]].callbacks[i]);
            }
        }
    };

    Wampy.prototype._renewRegistrations = function () {
        var rpcs = this._rpcRegs,
            rn = this._rpcNames,
            r;

        this._rpcRegs = {};
        this._rpcNames = [];

        r = rn.length;
        while (r--) {
            this.register(rn[r], { rpc: rpcs[rn[r]].callbacks[0] });
        }
    };

    /* Wampy public API */

    /**
     * Get or set Wampy options
     *
     * To get options - call without parameters
     * To set options - pass hash-table with options values
     *
     * @param {object} opts
     * @returns {*}
     */
    Wampy.prototype.options = function (opts) {
        if (opts === undefined) {
            return this._options;
        } else if (this._isPlainObject(opts)) {
            this._options = this._merge(this._options, opts);
            return this;
        }
    };

    /**
     * Get the status of last operation
     *
     * @returns {code, description}
     *      code: 0 - if operation was successful
     *      code > 0 - if error occurred
     *      description contains details about error
     *      reqId: last send request ID
     */
    Wampy.prototype.getOpStatus = function () {
        return this._cache.opStatus;
    };

    /**
     * Get the WAMP Session ID
     *
     * @returns Session ID
     */
    Wampy.prototype.getSessionId = function () {
        return this._cache.sessionId;
    };

    /**
     * Connect to server
     * @param {string} url New url (optional)
     * @returns {Wampy}
     */
    Wampy.prototype.connect = function (url) {
        if (url) {
            this._url = url;
        }

        if (this._options.realm) {
            this._setWsProtocols();
            this._ws = getWebSocket(this._url, this._protocols);
            if (this._ws) {
                this._initWsCallbacks();
            } else {
                this._cache.opStatus = WAMP_ERROR_MSG.NO_WS_URL;
            }
        } else {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_REALM;
        }

        return this;
    };

    /**
     * Disconnect from server
     * @returns {Wampy}
     */
    Wampy.prototype.disconnect = function () {
        if (this._cache.sessionId) {
            // need to send goodbye message to server
            this._cache.isSayingGoodbye = true;
            this._send([WAMP_MSG_SPEC.GOODBYE, {}, 'wamp.error.system_shutdown']);
        } else if (this._ws){
            this._ws.close();
        }

        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;

        return this;
    };

    /**
     * Abort WAMP session establishment
     *
     * @returns {Wampy}
     */
    Wampy.prototype.abort = function () {

        if (!this._cache.sessionId && this._ws.readyState === 1) {
            this._send([WAMP_MSG_SPEC.ABORT, {}, 'wamp.error.abort']);
            this._cache.sessionId = null;
        }

        this._ws.close();
        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;

        return this;
    };

    /**
     * Subscribe to a topic on a broker
     *
     * @param {string} topicURI
     * @param {function|object} callbacks - if it is a function - it will be treated as published event callback
     *                          or it can be hash table of callbacks:
     *                          { onSuccess: will be called when subscribe would be confirmed
     *                            onError: will be called if subscribe would be aborted
     *                            onEvent: will be called on receiving published event }
     *
     * @returns {Wampy}
     */
    Wampy.prototype.subscribe = function (topicURI, callbacks) {
        var reqId;

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.broker) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_BROKER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._validateURI(topicURI)) {
            this._cache.opStatus = WAMP_ERROR_MSG.URI_ERROR;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (typeof callbacks === 'function') {
            callbacks = { onEvent: callbacks };
        } else if (this._isPlainObject(callbacks) && callbacks.onEvent !== undefined) {

        } else {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_CALLBACK_SPEC;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._subscriptions[topicURI] || !this._subscriptions[topicURI].callbacks.length) {
            // no such subscription or processing unsubscribing

            reqId = this._getReqId();

            this._requests[reqId] = {
                topic: topicURI,
                callbacks: callbacks
            };

            // WAMP SPEC: [SUBSCRIBE, Request|id, Options|dict, Topic|uri]
            this._send([WAMP_MSG_SPEC.SUBSCRIBE, reqId, {}, topicURI]);

        } else {    // already have subscription to this topic
            // There is no such callback yet
            if (this._subscriptions[topicURI].callbacks.indexOf(callbacks.onEvent) < 0) {
                this._subscriptions[topicURI].callbacks.push(callbacks.onEvent);
            }

            if (callbacks.onSuccess) {
                callbacks.onSuccess();
            }
        }

        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
        this._cache.opStatus.reqId = reqId;
        return this;
    };

    /**
     * Unsubscribe from topic
     * @param {string} topicURI
     * @param {function|object} callbacks - if it is a function - it will be treated as
     *                          published event callback to remove or it can be hash table of callbacks:
     *                          { onSuccess: will be called when unsubscribe would be confirmed
     *                            onError: will be called if unsubscribe would be aborted
     *                            onEvent: published event callback to remove }
     * @returns {Wampy}
     */
    Wampy.prototype.unsubscribe = function (topicURI, callbacks) {
        var reqId, i = -1;

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.broker) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_BROKER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (this._subscriptions[topicURI]) {

            reqId = this._getReqId();

            if (callbacks === undefined) {
                this._subscriptions[topicURI].callbacks = [];
                callbacks = {};
            } else if (typeof callbacks === 'function') {
                i = this._subscriptions[topicURI].callbacks.indexOf(callbacks);
                callbacks = {};
            } else if (callbacks.onEvent && typeof callbacks.onEvent === 'function') {
                i = this._subscriptions[topicURI].callbacks.indexOf(callbacks.onEvent);
            } else {
                this._subscriptions[topicURI].callbacks = [];
            }

            if (i >= 0) {
                this._subscriptions[topicURI].callbacks.splice(i, 1);
            }

            if (this._subscriptions[topicURI].callbacks.length) {
                // There are another callbacks for this topic
                this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
                return this;
            }

            this._requests[reqId] = {
                topic: topicURI,
                callbacks: callbacks
            };

            // WAMP_SPEC: [UNSUBSCRIBE, Request|id, SUBSCRIBED.Subscription|id]
            this._send([WAMP_MSG_SPEC.UNSUBSCRIBE, reqId, this._subscriptions[topicURI].id]);

        } else {
            this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_UNSUBSCRIBE;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
        this._cache.opStatus.reqId = reqId;
        return this;
    };

    /**
     * Publish a event to topic
     * @param {string} topicURI
     * @param {string|number|Array|object} payload - optional parameter.
     * @param {object} callbacks - optional hash table of callbacks:
     *                          { onSuccess: will be called when publishing would be confirmed
     *                            onError: will be called if publishing would be aborted }
     * @param {object} advancedOptions - optional parameter. Must include any or all of the options:
     *                          { exclude: integer|array WAMP session id(s) that won't receive a published event,
     *                                      even though they may be subscribed
     *                            eligible: integer|array WAMP session id(s) that are allowed
     *                                      to receive a published event
     *                            exclude_me: bool flag of receiving publishing event by initiator
     *                            disclose_me: bool flag of disclosure of publisher identity (its WAMP session ID)
     *                                      to receivers of a published event }
     * @returns {Wampy}
     */
    Wampy.prototype.publish = function (topicURI, payload, callbacks, advancedOptions) {
        var reqId, msg, options = {}, err = false;

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.broker) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_BROKER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._validateURI(topicURI)) {
            this._cache.opStatus = WAMP_ERROR_MSG.URI_ERROR;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (this._isPlainObject(callbacks)) {
            options.acknowledge = true;
        }

        if (advancedOptions !== undefined) {

            if (this._isPlainObject(advancedOptions)) {
                if (advancedOptions.exclude){
                    if (this._isArray(advancedOptions.exclude)) {
                        options.exclude = advancedOptions.exclude;
                    } else if (typeof advancedOptions.exclude === 'number') {
                        options.exclude = [advancedOptions.exclude];
                    } else {
                        err = true;
                    }
                }

                if (advancedOptions.eligible){
                    if (this._isArray(advancedOptions.eligible)) {
                        options.eligible = advancedOptions.eligible;
                    } else if (typeof advancedOptions.eligible === 'number') {
                        options.eligible = [advancedOptions.eligible];
                    } else {
                        err = true;
                    }
                }

                if (advancedOptions.hasOwnProperty('exclude_me')) {
                    options.exclude_me = advancedOptions.exclude_me !== false;
                }

                if (advancedOptions.hasOwnProperty('disclose_me')) {
                    options.disclose_me = advancedOptions.disclose_me === true;
                }

            } else {
                err = true;
            }

            if (err) {
                this._cache.opStatus = WAMP_ERROR_MSG.INVALID_PARAM;

                if (this._isPlainObject(callbacks) && callbacks.onError) {
                    callbacks.onError(this._cache.opStatus.description);
                }

                return this;
            }
        }

        reqId = this._getReqId();

        switch (arguments.length) {
            case 1:
                // WAMP_SPEC: [PUBLISH, Request|id, Options|dict, Topic|uri]
                msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI];
                break;
            case 2:
                // WAMP_SPEC: [PUBLISH, Request|id, Options|dict, Topic|uri, Arguments|list (, ArgumentsKw|dict)]
                if (this._isArray(payload)) {
                    msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI, payload];
                } else if (this._isPlainObject(payload)) {
                    msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI, [], payload];
                } else {    // assume it's a single value
                    msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI, [payload]];
                }
                break;
            default:
                this._requests[reqId] = {
                            topic: topicURI,
                            callbacks: callbacks
                        };

                // WAMP_SPEC: [PUBLISH, Request|id, Options|dict, Topic|uri, Arguments|list (, ArgumentsKw|dict)]
                if (this._isArray(payload)) {
                    msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI, payload];
                } else if (this._isPlainObject(payload)) {
                    msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI, [], payload];
                } else {    // assume it's a single value
                    msg = [WAMP_MSG_SPEC.PUBLISH, reqId, options, topicURI, [payload]];
                }
                break;
        }

        this._send(msg);
        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
        this._cache.opStatus.reqId = reqId;
        return this;
    };

    /**
     * Remote Procedure Call
     * @param {string} topicURI
     * @param {string|number|Array|object} payload - can be either a value of any type or null
     * @param {function|object} callbacks - if it is a function - it will be treated as result callback function
     *                          or it can be hash table of callbacks:
     *                          { onSuccess: will be called with result on successful call
     *                            onError: will be called if invocation would be aborted }
     * @param {object} advancedOptions - optional parameter. Must include any or all of the options:
     *                          { exclude: integer|array WAMP session id(s) providing an explicit list of
     *                                  (potential) Callees that a call won't be forwarded to, even though
     *                                  they might be registered
     *                            eligible: integer|array WAMP session id(s) providing an explicit list of
     *                                  (potential) Callees that are (potentially) forwarded the call issued
     *                            exclude_me: bool flag of potentially forwarding call to caller
     *                                  if he is registered as callee
     *                            disclose_me: bool flag of disclosure of Caller identity (WAMP session ID)
     *                                  to endpoints of a routed call
     *                            receive_progress: bool flag for receiving progressive results. In this case
     *                                  onSuccess function will be called every time on receiving result }
     * @returns {Wampy}
     */
    Wampy.prototype.call = function (topicURI, payload, callbacks, advancedOptions) {
        var reqId, msg, options = {}, err = false;

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.dealer) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_DEALER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._validateURI(topicURI)) {
            this._cache.opStatus = WAMP_ERROR_MSG.URI_ERROR;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (typeof callbacks === 'function') {
            callbacks = { onSuccess: callbacks };
        } else if (this._isPlainObject(callbacks) && callbacks.onSuccess !== undefined) {

        } else {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_CALLBACK_SPEC;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (advancedOptions !== undefined) {

            if (this._isPlainObject(advancedOptions)) {
                if (advancedOptions.exclude){
                    if (this._isArray(advancedOptions.exclude)) {
                        options.exclude = advancedOptions.exclude;
                    } else if (typeof advancedOptions.exclude === 'number') {
                        options.exclude = [advancedOptions.exclude];
                    } else {
                        err = true;
                    }
                }

                if (advancedOptions.eligible){
                    if (this._isArray(advancedOptions.eligible)) {
                        options.eligible = advancedOptions.eligible;
                    } else if (typeof advancedOptions.eligible === 'number') {
                        options.eligible = [advancedOptions.eligible];
                    } else {
                        err = true;
                    }
                }

                if (advancedOptions.hasOwnProperty('exclude_me')) {
                    options.exclude_me = advancedOptions.exclude_me !== false;
                }

                if (advancedOptions.hasOwnProperty('disclose_me')) {
                    options.disclose_me = advancedOptions.disclose_me === true;
                }

                if (advancedOptions.hasOwnProperty('receive_progress')) {
                    options.receive_progress = advancedOptions.receive_progress === true;
                }

            } else {
                err = true;
            }

            if (err) {
                this._cache.opStatus = WAMP_ERROR_MSG.INVALID_PARAM;

                if (this._isPlainObject(callbacks) && callbacks.onError) {
                    callbacks.onError(this._cache.opStatus.description);
                }

                return this;
            }
        }

        do {
            reqId = this._getReqId();
        } while (reqId in this._calls);

        this._calls[reqId] = callbacks;

        // WAMP SPEC: [CALL, Request|id, Options|dict, Procedure|uri, (Arguments|list, ArgumentsKw|dict)]
        if (payload === null) {
            msg = [WAMP_MSG_SPEC.CALL, reqId, options, topicURI];
        } else {
            if (this._isArray(payload)) {
                msg = [WAMP_MSG_SPEC.CALL, reqId, options, topicURI, payload];
            } else if (this._isPlainObject(payload)) {
                msg = [WAMP_MSG_SPEC.CALL, reqId, options, topicURI, [], payload];
            } else {    // assume it's a single value
                msg = [WAMP_MSG_SPEC.CALL, reqId, options, topicURI, [payload]];
            }
        }

        this._send(msg);
        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
        this._cache.opStatus.reqId = reqId;
        return this;
    };

    /**
     * RPC invocation cancelling
     *
     * @param {int} reqId RPC call request ID
     * @param {function|object} callbacks - if it is a function - it will be called if successfully
     *                          sent canceling message or it can be hash table of callbacks:
     *                          { onSuccess: will be called if successfully sent canceling message
     *                            onError: will be called if some error occurred }
     * @param {object} advancedOptions - optional parameter. Must include any or all of the options:
     *                          { mode: string|one of the possible modes:
     *                                  "skip" | "kill" | "killnowait". Skip is default.
      *                          }
     *
     * @returns {Wampy}
     */
    Wampy.prototype.cancel = function (reqId, callbacks, advancedOptions) {
        var options = { mode: 'skip' };

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.dealer) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_DEALER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!reqId || !this._calls[reqId]) {
            this._cache.opStatus = WAMP_ERROR_MSG.NON_EXIST_RPC_REQ_ID;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (advancedOptions !== undefined) {
            if (this._isPlainObject(advancedOptions)) {
                if (advancedOptions.hasOwnProperty('mode')) {
                    options.mode = /skip|kill|killnowait/.test(advancedOptions.mode) ? advancedOptions.mode : 'skip' ;
                }
            }
        }

        // WAMP SPEC: [CANCEL, CALL.Request|id, Options|dict]
        this._send([WAMP_MSG_SPEC.CANCEL, reqId, options]);

        if (callbacks.onSuccess) {
            callbacks.onSuccess();
        }

        this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
        this._cache.opStatus.reqId = reqId;
        return this;

    };

    /**
     * RPC registration for invocation
     * @param {string} topicURI
     * @param {function|object} callbacks - if it is a function - it will be treated as rpc itself
     *                          or it can be hash table of callbacks:
     *                          { rpc: registered procedure
     *                            onSuccess: will be called on successful registration
     *                            onError: will be called if registration would be aborted }
     * @returns {Wampy}
     */
    Wampy.prototype.register = function (topicURI, callbacks) {
        var reqId;

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.dealer) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_DEALER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._validateURI(topicURI)) {
            this._cache.opStatus = WAMP_ERROR_MSG.URI_ERROR;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (typeof callbacks === 'function') {
            callbacks = { rpc: callbacks };
        } else if (this._isPlainObject(callbacks) && callbacks.rpc !== undefined) {

        } else {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_CALLBACK_SPEC;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._rpcRegs[topicURI] || !this._rpcRegs[topicURI].callbacks.length) {
            // no such registration or processing unregistering

            reqId = this._getReqId();

            this._requests[reqId] = {
                topic: topicURI,
                callbacks: callbacks
            };

            // WAMP SPEC: [REGISTER, Request|id, Options|dict, Procedure|uri]
            this._send([WAMP_MSG_SPEC.REGISTER, reqId, {}, topicURI]);
            this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
            this._cache.opStatus.reqId = reqId;
        } else {    // already have registration with such topicURI
            this._cache.opStatus = WAMP_ERROR_MSG.RPC_ALREADY_REGISTERED;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

        }

        return this;

    };

    /**
     * RPC unregistration for invocation
     * @param {string} topicURI
     * @param {function|object} callbacks - if it is a function, it will be called on successful unregistration
     *                          or it can be hash table of callbacks:
     *                          { onSuccess: will be called on successful unregistration
     *                            onError: will be called if unregistration would be aborted }
     * @returns {Wampy}
     */
    Wampy.prototype.unregister = function (topicURI, callbacks) {
        var reqId;

        if (this._cache.sessionId && !this._cache.server_wamp_features.roles.dealer) {
            this._cache.opStatus = WAMP_ERROR_MSG.NO_DEALER;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (!this._validateURI(topicURI)) {
            this._cache.opStatus = WAMP_ERROR_MSG.URI_ERROR;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

            return this;
        }

        if (typeof callbacks === 'function') {
            callbacks = { onSuccess: callbacks };
        }

        if (this._rpcRegs[topicURI]) {   // there is such registration

            reqId = this._getReqId();

            this._requests[reqId] = {
                topic: topicURI,
                callbacks: callbacks
            };

            // WAMP SPEC: [UNREGISTER, Request|id, REGISTERED.Registration|id]
            this._send([WAMP_MSG_SPEC.UNREGISTER, reqId, this._rpcRegs[topicURI].id]);
            this._cache.opStatus = WAMP_ERROR_MSG.SUCCESS;
            this._cache.opStatus.reqId = reqId;
        } else {    // already have registration with such topicURI
            this._cache.opStatus = WAMP_ERROR_MSG.RPC_ALREADY_REGISTERED;

            if (this._isPlainObject(callbacks) && callbacks.onError) {
                callbacks.onError(this._cache.opStatus.description);
            }

        }

        return this;

    };

    return Wampy;

});
