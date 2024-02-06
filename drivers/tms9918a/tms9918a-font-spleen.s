;
; Copyright (c) 2018-2021 Frederic Cambus <fcambus@openbsd.org>
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
; SUCH DAMAGE.
;

;
; Converted from NetBSD's sys/dev/wsfont/spleen5x8.h
;

VDP_tty_font_tiles
	fcc	0	; cursor character
	fcc	$a8	; *.*.*...
	fcc	$50	; .*.*....
	fcc	$a8	; *.*.*...
	fcc	$50	; .*.*....
	fcc	$a8	; *.*.*...
	fcc	$50	; .*.*....
	fcc	$a8	; *.*.*...
	fcc	$50	; .*.*....

	fcc	32
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	33
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........

	fcc	34
	fcc	$50	; .*.*....
	fcc	$50	; .*.*....
	fcc	$50	; .*.*....
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	35
	fcc	$00	; ........
	fcc	$50	; .*.*....
	fcc	$f8	; *****...
	fcc	$50	; .*.*....
	fcc	$50	; .*.*....
	fcc	$f8	; *****...
	fcc	$50	; .*.*....
	fcc	$00	; ........

	fcc	36
	fcc	$20	; ..*.....
	fcc	$70	; .***....
	fcc	$a0	; *.*.....
	fcc	$60	; .**.....
	fcc	$30	; ..**....
	fcc	$30	; ..**....
	fcc	$e0	; ***.....
	fcc	$20	; ..*.....

	fcc	37
	fcc	$10	; ...*....
	fcc	$90	; *..*....
	fcc	$a0	; *.*.....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$50	; .*.*....
	fcc	$90	; *..*....
	fcc	$80	; *.......

	fcc	38
	fcc	$20	; ..*.....
	fcc	$50	; .*.*....
	fcc	$50	; .*.*....
	fcc	$60	; .**.....
	fcc	$a8	; *.*.*...
	fcc	$90	; *..*....
	fcc	$68	; .**.*...
	fcc	$00	; ........

	fcc	39
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	40
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$20	; ..*.....
	fcc	$10	; ...*....

	fcc	41
	fcc	$40	; .*......
	fcc	$20	; ..*.....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......

	fcc	42
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$f0	; ****....
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	43
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$f8	; *****...
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$00	; ........

	fcc	44
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$40	; .*......

	fcc	45
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	46
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........

	fcc	47
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$80	; *.......
	fcc	$80	; *.......

	fcc	48
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$b0	; *.**....
	fcc	$d0	; **.*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	49
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$60	; .**.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	50
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$10	; ...*....
	fcc	$60	; .**.....
	fcc	$80	; *.......
	fcc	$f0	; ****....
	fcc	$00	; ........

	fcc	51
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$20	; ..*.....
	fcc	$10	; ...*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	52
	fcc	$00	; ........
	fcc	$80	; *.......
	fcc	$a0	; *.*.....
	fcc	$a0	; *.*.....
	fcc	$f0	; ****....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$00	; ........

	fcc	53
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$80	; *.......
	fcc	$e0	; ***.....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	54
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$80	; *.......
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	55
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$00	; ........

	fcc	56
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	57
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$10	; ...*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	58
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........

	fcc	59
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$40	; .*......

	fcc	60
	fcc	$00	; ........
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$20	; ..*.....
	fcc	$10	; ...*....
	fcc	$00	; ........

	fcc	61
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	62
	fcc	$00	; ........
	fcc	$40	; .*......
	fcc	$20	; ..*.....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$00	; ........

	fcc	63
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$00	; ........
	fcc	$40	; .*......
	fcc	$00	; ........

	fcc	64
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$b0	; *.**....
	fcc	$b0	; *.**....
	fcc	$80	; *.......
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	65
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	66
	fcc	$00	; ........
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	67
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	68
	fcc	$00	; ........
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	69
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$e0	; ***.....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	70
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$e0	; ***.....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$00	; ........

	fcc	71
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$b0	; *.**....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	72
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	73
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	74
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$c0	; **......
	fcc	$00	; ........

	fcc	75
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	76
	fcc	$00	; ........
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	77
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	78
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$d0	; **.*....
	fcc	$d0	; **.*....
	fcc	$b0	; *.**....
	fcc	$b0	; *.**....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	79
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	80
	fcc	$00	; ........
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$00	; ........

	fcc	81
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$30	; ..**....

	fcc	82
	fcc	$00	; ........
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	83
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$60	; .**.....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	84
	fcc	$00	; ........
	fcc	$f8	; *****...
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$00	; ........

	fcc	85
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	86
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	87
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	88
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	89
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$10	; ...*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	90
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$80	; *.......
	fcc	$f0	; ****....
	fcc	$00	; ........

	fcc	91
	fcc	$70	; .***....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$70	; .***....

	fcc	92
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$10	; ...*....
	fcc	$10	; ...*....

	fcc	93
	fcc	$70	; .***....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$70	; .***....

	fcc	94
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$50	; .*.*....
	fcc	$88	; *...*...
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	95
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$f0	; ****....

	fcc	96
	fcc	$40	; .*......
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	97
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$10	; ...*....
	fcc	$70	; .***....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	98
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	99
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	100
	fcc	$10	; ...*....
	fcc	$10	; ...*....
	fcc	$70	; .***....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	101
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$80	; *.......
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	102
	fcc	$30	; ..**....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$e0	; ***.....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$00	; ........

	fcc	103
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$10	; ...*....
	fcc	$e0	; ***.....

	fcc	104
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	105
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$30	; ..**....
	fcc	$00	; ........

	fcc	106
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$00	; ........
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$c0	; **......

	fcc	107
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$90	; *..*....
	fcc	$a0	; *.*.....
	fcc	$c0	; **......
	fcc	$a0	; *.*.....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	108
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$30	; ..**....
	fcc	$00	; ........

	fcc	109
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	110
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	111
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	112
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$e0	; ***.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$e0	; ***.....
	fcc	$80	; *.......
	fcc	$80	; *.......

	fcc	113
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$10	; ...*....
	fcc	$10	; ...*....

	fcc	114
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$90	; *..*....
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$80	; *.......
	fcc	$00	; ........

	fcc	115
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$70	; .***....
	fcc	$80	; *.......
	fcc	$60	; .**.....
	fcc	$10	; ...*....
	fcc	$e0	; ***.....
	fcc	$00	; ........

	fcc	116
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$e0	; ***.....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$30	; ..**....
	fcc	$00	; ........

	fcc	117
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$00	; ........

	fcc	118
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$60	; .**.....
	fcc	$00	; ........

	fcc	119
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$f0	; ****....
	fcc	$f0	; ****....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	120
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$60	; .**.....
	fcc	$60	; .**.....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$00	; ........

	fcc	121
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$90	; *..*....
	fcc	$70	; .***....
	fcc	$10	; ...*....
	fcc	$e0	; ***.....

	fcc	122
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$f0	; ****....
	fcc	$10	; ...*....
	fcc	$20	; ..*.....
	fcc	$40	; .*......
	fcc	$f0	; ****....
	fcc	$00	; ........

	fcc	123
	fcc	$30	; ..**....
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$c0	; **......
	fcc	$c0	; **......
	fcc	$40	; .*......
	fcc	$40	; .*......
	fcc	$30	; ..**....

	fcc	124
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....

	fcc	125
	fcc	$c0	; **......
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$30	; ..**....
	fcc	$30	; ..**....
	fcc	$20	; ..*.....
	fcc	$20	; ..*.....
	fcc	$c0	; **......

	fcc	126
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$48	; .*..*...
	fcc	$b0	; *.**....
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........

	fcc	127
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
	fcc	$00	; ........
VDP_tty_font_tiles_end
VDP_tty_font_tiles_size	equ	(VDP_tty_font_tiles_end - VDP_tty_font_tiles)
VDP_tty_font_ntiles	equ	(VDP_tty_font_tiles_size / 9)
