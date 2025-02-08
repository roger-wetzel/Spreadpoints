; SPREADPOINT
; SPREADPOINTS (AMIGA, PAL, OCS, 68000, 512 KB)
; (C) 2025 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o spreadpoints -nosym spreadpoints.s

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

inviswidth	equ	0
pwidth		equ	40+inviswidth ; clipping zone on the left and right side

numchars	equ	10	; "SPREADOINT"
cbwidth		equ	4	; char buffer width (4 bytes = 2 words -> blitter)
charheight	equ	24	; px
charsourcesize	equ	3*charheight ; note: width 3 bytes
rotationsteps	equ	72

numrings	equ	6

pheight		equ	256+16 ; px
psize		equ	pheight*pwidth
invisheight	equ	24 ; height of clipping zone top and bottom (and middle)
invissize	equ	invisheight*pwidth
center		equ	psize/2+((pwidth-inviswidth)/2)
bitplanessize	equ	3*invissize+(2*psize)

taxirows	equ	10
taxicolumns	equ	11
taxideltax	equ	26
taxideltay	equ	26
taxishadowshift	equ	4

scrollpwidth	equ	1024 ; 1024*8 = 8192 ($2000) enough (measured $1e00)
scrollspeed	equ	4

logoheight	equ	165
logopwidth	equ	40
logopheight	equ	logoheight
logopsize	equ	logopheight*logopwidth ; noise planes only

; profiling
numbers		equ	0

framelaps	equ	0
availablemem	equ	0
timing		equ	0
testing		equ	0

; DMACON
SET		equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
BLTPRI		equ	1<<10		; Blitter DMA priority (over CPU) "blitter nasty"
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bitplane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA
SPREN		equ	1<<5		; Sprite DMA


*------	MACROS ----------------------------------------------------------------*

; actor bits
actor_player		equ	0
actor_prepare_demo	equ	1
actor_lsp		equ	2
actor_prepare_txt	equ	3
actor_rings		equ	4
actor_logo		equ	5
actor_logo_fading	equ	6
actor_metaballs		equ	7
actor_metaballsshow	equ	8
actor_metaballshide	equ	9
actor_taxi		equ	10
actor_clear_bitplanes	equ	11


; note: elseif does not work correctly
	macro CHECKACTOR
	if \1<8
		btst	#\1,v_actors+3(a5)
		mexit
	endif
	if \1<16
		btst	#\1-8,v_actors+2(a5)
		mexit
	endif
	if \1<24
		btst	#\1-16,v_actors+1(a5)
		mexit
	endif
	if \1<32
		btst	#\1-24,v_actors(a5)
		mexit
	endif
	if \1>=32
		fail Out of range
	endif
	endm

	macro STARTACTOR
	if \1<8
		bset	#\1,v_actors+3(a5)
		mexit
	endif
	if \1<16
		bset	#\1-8,v_actors+2(a5)
		mexit
	endif
	if \1<24
		bset	#\1-16,v_actors+1(a5)
		mexit
	endif
	if \1<32
		bset	#\1-24,v_actors(a5)
		mexit
	endif
	if \1>=32
		fail Out of range
	endif
	endm

	macro STOPACTOR
	if \1<8
		bclr	#\1,v_actors+3(a5)
		mexit
	endif
	if \1<16
		bclr	#\1-8,v_actors+2(a5)
		mexit
	endif
	if \1<24
		bclr	#\1-16,v_actors+1(a5)
		mexit
	endif
	if \1<32
		bclr	#\1-24,v_actors(a5)
		mexit
	endif
	if \1>=32
		fail Out of range
	endif
	endm


*------	ENTRY -----------------------------------------------------------------*

base	movem.l	d0-a6,-(a7)		;
	bsr	allocandinit		;
	bne	.exit			; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6	;
	move.l	#MEMF_CHIP,d1		;
	jsr	AvailMem(a6)		;
	move.l	b_vars(pc),a5		;
	move.l	d0,v_number(a5)		; free (available) chip memory
	endif

	move.l	AbsExecBase.w,a6	;
	lea	.gfx(pc),a1		;
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move.l 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
	move.l 	50(a6),-(a7)		; copper list 2
	sub.l	a1,a1			;
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	AbsExecBase.w,a6	;
	jsr	Forbid(a6)		;
	
	lea	custom,a6		;

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3

	bsr	waitblitter		;
	bsr	waitraster		; avoid flickering (?) and sprite dma "bug"

	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		; disable all DMAs

	bsr	volumetozero		;

	moveq	#0,d0			; color
	moveq	#32-1,d7		;
	lea	$180(a6),a0		;
.black	move.w	d0,(a0)+		;
	dbf	d7,.black		;		

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		;
	beq	.ledstate		;
	not.w	(a7)			;
.ledstate
	bset	#1,$bfe001		; LED dark

*------	START -----------------------------------------------------------------*

	lea	irq3(pc),a0		;
	move.l	a0,$6c.w		;

	bsr	cmdlogo			; start with logo effect

	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN+SPREN,$96(a6) ;
;	move.w	#SET+DMAEN+BLTEN+COPEN,$96(a6) ;

	move.w	#$c020,$9a(a6)		; enable vertb interrupt

	sub.l	a2,a2			; VBR 68000 (improve for > 68000)
	bsr	LSP_MusicDriver_CIA_Start ;

*------	IDLE LOOP -------------------------------------------------------------*

	bsr	backgroundtasks		;
	bsr	waitraster		;
	
*------	RESTORE STATE AND EXIT ------------------------------------------------*
	
	tst.w	(a7)+			; restore state
	bne	.leddark		;
	bclr	#1,$bfe001		; LED bright
.leddark
	bsr	waitblitter		;
	bsr	waitraster		;

	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		;

	bsr	LSP_MusicDriver_CIA_Stop ;
	bsr	volumetozero		;

	move.l	(a7)+,$6c.w		; 
	move.w	(a7)+,d0		;
	or.w	#$c000,d0		;
	move.w	d0,$9a(a6)		;
	move.w	(a7)+,d0		;
	or.w	#$8000,d0		;
	move.w	d0,$96(a6)		;

	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	;
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		;

	bsr	dealloc			;
.exit	movem.l	(a7)+,d0-a6		;
	moveq	#0,d0			;
	rts				;

.gfx	dc.b	"graphics.library",0
	even


*------	VOLUME TO ZERO --------------------------------------------------------*

volumetozero
	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		;
	move.w	d0,$b8(a6)		;
	move.w	d0,$c8(a6)		;
	move.w	d0,$d8(a6)		;
	rts				;


*------	VARS ------------------------------------------------------------------*

; # = do not change order
	rsreset
v_doquit	rs.b	1	; signal quit
v_wait		rs.b	1

v_mirror	rs.b	256	; mirrored byte values
v_rings		rs.b	numrings*sizeofring

v_adjcharbuffer	rs.l	1

v_aball1	rs.w	1
v_bball1	rs.w	1
v_aball2	rs.w	1
v_bball2	rs.w	1
v_aball3	rs.w	1
v_bball3	rs.w	1
v_aball4	rs.w	1
v_bball4	rs.w	1

v_dbclist1a	rs.l	1	; #
v_dbclist1b	rs.l	1	; #

v_taxia		rs.w	1
v_taxib		rs.w	1
v_taxic		rs.w	1
v_taxid		rs.w	1

v_taxideltas	rs.w	0	; marker only
v_taxideltaa	rs.w	1	; #
v_taxideltab	rs.w	1	; #
v_taxideltac	rs.w	1	; #
v_taxideltad	rs.w	1	; #

v_taxicurrenty	rs.w	1	; #

v_taxix		rs.w	1	; #
v_taxiy		rs.w	1	; #

v_taxidata	rs.w	taxirows*taxicolumns*(1+1+2)

		rs.b	20*bufferw	; safety zone
v_buffer	rs.b	bufferh*bufferw
		rs.b	20*bufferw	; safety zone

v_mbcolsoffset	rs.w	1
v_patchxoffset	rs.w	1
v_patchyoffset	rs.w	1

v_scrollpos	rs.w	1
v_bplcon1	rs.l	1
v_bpl1pt	rs.l	1
v_bpl2pt	rs.l	1
v_textcolor	rs.l	1
v_textcolindex	rs.w	1

v_actors	rs.l	1
v_cmdspointer	rs.l	1

v_bitplane	rs.l	1
v_db1a2a	rs.l	1	; #
v_db1b2b	rs.l	1	; #
v_adjdb1a2a	rs.l	1	; #
v_adjdb1b2b	rs.l	1	; #

v_ringscolindex	rs.w	1
v_logocolindex	rs.w	1
v_logofadestate	rs.w	1

v_frame		rs.w	1	; frame counter

	if numbers
v_number	rs.l	1	; test value
v_waitcount	rs.w	1
v_framelap	rs.w	1
	endif

v_zero		rs.l	10	; 10 registers with value 0 for cpu cls

v_taxicollut	rs.b	256
v_setrings	rs.b	1
v_scroll	rs.b	1	; 0=off otherwise=on

sizeofvars	rs.w	0

	rsreset
ring_datap	rs.l	1	; #
ring_range	rs.w	1	; #
ring_index	rs.w	1	; #
ring_numchars	rs.w	1	; #
ring_delta	rs.w	1	; #
ring_advance	rs.w	1
ring_id		rs.b	1
		rs.b	1	; dummy (alignment)
sizeofring	rs.w	0


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
	if numbers
printnumber
	move.l	v_db1a2a(a5),d6		;
	beq	.stop			;
	move.l	d6,a0			;
	add.w	d1,a0			;
 	moveq	#8-1,d7			; 8 digits
.loop	move.w	d0,d1			; number
	and.w	#$000f,d1		; mask digit out
	asl.w	#3,d1			; offset to font data
	lea	.digits(pc,d1.w),a1	;
	move.l	a0,a2			;
	rept 5
	move.b	(a1)+,(a2)		; print digit
	add.w	#pwidth,a2		; next line
	endr
	asr.l	#4,d0			; next digit
	subq.w	#1,a0			; next x position
	dbf	d7,.loop		;
.stop	rts				;

.digits	dc.b	%11111000	; 0
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%00100000	; 1
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 2
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 3
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%10001000	; 4
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%00001000
	ds.b	3

	dc.b	%11111000	; 5
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 6
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 7
	dc.b	%00001000
	dc.b	%00010000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 8
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 9
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; A
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%10001000
	ds.b	3

	dc.b	%11110000	; B
	dc.b	%10001000
	dc.b	%11110000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; C
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11110000	; D
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; E
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; F
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%10000000
	ds.b	3

	endif


*------	WAIT BLITTER ----------------------------------------------------------*

waitblitter
	move.w	#SET+DMAEN+BLTPRI,$96(a6)	;
	btst	#14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait	btst	#14-8,$02(a6)			;
	bne	.wait				;
	move.w	#BLTPRI,$96(a6)			;
	rts					;


*------	WAIT RASTER -----------------------------------------------------------*

waitraster
.wait	move.l	$04(a6),d0			;
	and.l	#$0001ff00,d0			;
	cmp.l	#312<<8,d0			; line to wait for
	bne	.wait				;
	rts					;


*------	BACKGROUND TASKS ------------------------------------------------------*

backgroundtasks
.idle	
	CHECKACTOR actor_prepare_demo
	beq	.txt				;

	lea	text(pc),a0			;
	move.l	b_scrollplane(pc),a1		;	
	moveq	#1,d4				;
	ror.b	d4,d4				;
	move.l	#scrollpwidth,d5		;
	bsr	printtext			;

	bsr	rotatefont			;

	bsr	generaterings			;
	tst.b	v_doquit(a5)			;
	bne	.quit				;
	STOPACTOR actor_prepare_demo

.txt	CHECKACTOR actor_prepare_txt
	beq	.clearb				;
	move.l	b_scrollplane(pc),a1		;
	move.l	a1,a2				;
	move.w	#scrollpwidth*visiblefontheight/2-1,d7 ;
.clear	clr.w	(a2)+				;
	dbf	d7,.clear			;
	lea	greetingslist(pc),a0		;
	moveq	#1,d4				;
	ror.b	d4,d4				;
	move.l	#scrollpwidth,d5		;
	bsr	printtext			;
	STOPACTOR actor_prepare_txt

	if testing
;	move.w	v_frame(a5),v_number(a5)	;
	endif

.clearb	CHECKACTOR actor_clear_bitplanes
	beq	.done				;
	move.l	b_bitplanes(pc),a0		;
	move.w	#bitplanessize/4-1,d7		;
.cl	clr.l	(a0)+				;
	dbf	d7,.cl				;
	STOPACTOR actor_clear_bitplanes

.done	tst.b	v_doquit(a5)			;
	beq	.idle				;
.quit	move.w	#$0200,$100(a6)			; avoid flickering?
	rts					;


*------	IRQ3 ------------------------------------------------------------------*

irq3	movem.l	d0-a6,-(a7)			;
	move.l	b_vars(pc),a5			;
	lea	custom,a6			;

	CHECKACTOR actor_player
	beq	.metaballs			;
	bsr	play				;
.metaballs
	CHECKACTOR actor_metaballs
	beq	.rings				;
	bsr	metaballseffect			;
	bra	.eofx				;
.rings	CHECKACTOR actor_rings
	beq	.taxi				;
	bsr	ringseffect			;
	bra	.eofx				;
.taxi	CHECKACTOR actor_taxi
	beq	.logo				;
	bsr	taxieffect			;
	bra	.eofx				;
.logo	CHECKACTOR actor_logo
	beq	.eofx				;
	bsr	logoeffect			;
	bra	.nodb				; logo does not use double buffering

.eofx	movem.l	v_db1a2a(a5),d0-d3		; double buffering
	exg	d0,d1				; v_db1a2a <-> v_db1b2b
	exg	d2,d3				; v_adjdb1a2a <-> v_adjdb1b2b
	movem.l	d0-d3,v_db1a2a(a5)		;

.nodb
	if timing|numbers
	bsr	waitblitter			;
	endif

	if timing
	move.w	#$0440,$180(a6)			; dark yellow color indicates numbers consumption
	endif

	if numbers
	moveq	#0,d0				; value
	move.w	v_frame(a5),d0			;
	asl.l	#8,d0				;
	move.b	v_waitcount(a5),d0		;
	asl.l	#8,d0				;
	move.b	v_wait(a5),d0			;
	moveq	#8-1,d1				; pos
	bsr	printnumber			;

	if framelaps
	move.l	v_framelap(a5),d0		;
	move.w	#10*pwidth+8-1,d1		; pos second line
	bsr	printnumber			;
	endif
	endif

	addq.w	#1,v_frame(a5)			; advance frame number
	if framelaps
	addq.w	#1,v_framelap(a5)		;
	endif

	btst	#6,$bfe001			; left mouse button pressed?
	bne	.noquit				; (do NOT use seq)
	st	v_doquit(a5)			;
.noquit
	if timing
	move.w	#$0030,$180(a6)			; dark green color indicates free capacity
	endif

	moveq	#$0020,d0			; delete vertb request
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;

	movem.l	(a7)+,d0-a6			;
	rte					;


*------	LOGO EFFECT -----------------------------------------------------------*

logoeffect
	bsr	logofading			;

	move.l	b_clist5(pc),a2			;
	sub.l	v_bplcon1(a5),a2		;
	add.l	#clist5marker-clist5,a2		;
	bsr	scrolltext			;

	bsr	patch				;
	bsr	patch				;
	bsr	patch				;

	move.l	b_bitplanes(pc),a0		; Virgill noise (Thanks Virgill)
	lea	logopwidth(a0),a1		; next line
	moveq	#0,d0				;
	moveq	#-1,d1				;
	bsr	waitblitter			;
	move.l	d0,$60(a6)			; modulo C and B	
	move.l	d0,$64(a6)			; modulo A and D
	move.l	#$1f1e0000,$40(a6)		; $1 = Shift A by 1, $f ABCD, $1e (magic Virgill value)
	move.l	d1,$44(a6)			; first/last word mask
	move.l	a0,$50(a6)			; source A
	move.l	a1,$4c(a6)			; source B
	move.l	a0,$48(a6)			; source C
	move.l	a0,$54(a6)			; destination D
	move.w	#(2*logopheight)<<6+logopwidth>>1,$58(a6) ; bltsize and start
	rts


*------	NOISE PATCH -----------------------------------------------------------*

patch	move.l	b_bitplanes(pc),a0		;

	move.w	v_patchyoffset(a5),d1		;
	add.w	.yoffsets(pc,d1.w),a0		;
	addq.w	#2,v_patchyoffset(a5)		;
	cmp.w	#.yoffsetsend-.yoffsets,v_patchyoffset(a5) ;
	bne	.yinrange			;
	clr.w	v_patchyoffset(a5)		; reset
.yinrange

	move.w	v_patchxoffset(a5),d1		;
	add.w	.xoffsets(pc,d1.w),a0		;
	addq.w	#2,v_patchxoffset(a5)		;
	cmp.w	#.xoffsetsend-.xoffsets,v_patchxoffset(a5) ;
	bne	.xinrange			;
	clr.w	v_patchxoffset(a5)		; reset
.xinrange

	moveq	#-1,d0				; pattern
	moveq	#16-1,d7			; patch height
.set	move.w	d0,(a0)				;
	clr.w	logopsize(a0)			;
	add.w	#logopwidth,a0			; next line
	dbf	d7,.set				;
	rts					;

.yoffsets	
	dc.w	40*logopwidth
	dc.w	50*logopwidth
	dc.w	60*logopwidth
	dc.w	100*logopwidth
	dc.w	120*logopwidth
	dc.w	50*logopwidth
	dc.w	130*logopwidth
	dc.w	10*logopwidth
	dc.w	80*logopwidth
	dc.w	110*logopwidth
	dc.w	30*logopwidth
	dc.w	100*logopwidth
	dc.w	140*logopwidth
	dc.w	10*logopwidth
	dc.w	20*logopwidth
	dc.w	60*logopwidth
	dc.w	40*logopwidth
	dc.w	30*logopwidth
	dc.w	90*logopwidth
	dc.w	50*logopwidth
	dc.w	40*logopwidth
	dc.w	50*logopwidth
	dc.w	60*logopwidth
	dc.w	70*logopwidth
	dc.w	110*logopwidth
	dc.w	10*logopwidth
	dc.w	80*logopwidth
	dc.w	150*logopwidth
	dc.w	100*logopwidth
	dc.w	110*logopwidth
	dc.w	70*logopwidth
	dc.w	90*logopwidth
	dc.w	0*logopwidth
	dc.w	20*logopwidth
.yoffsetsend

.xoffsets
	dc.w	9*2,7*2,5*2,6*2,13*2,11*2,6*2,18*2
	dc.w	6*2,15*2,12*2,1*2,3*2,18*2,18*2,8*2
	dc.w	11*2,6*2,12*2,12*2,9*2,14*2,13*2,7*2
	dc.w	12*2,1*2,0*2,12*2,16*2,14*2,1*2,10*2
	dc.w	16*2,7*2,5*2,4*2,2*2,16*2,6*2,19*2
	dc.w	18*2,7*2,6*2,19*2,13*2,3*2,17*2,5*2
	dc.w	0*2,8*2,13*2,17*2,6*2,18*2,6*2,13*2
	dc.w	4*2,14*2,15*2,19*2,8*2,18*2,4*2,7*2
	dc.w	3*2,12*2,10*2,6*2,11*2,8*2,10*2,12*2
	dc.w	2*2,18*2,14*2,4*2,8*2,11*2,13*2,9*2
	dc.w	8*2,17*2,11*2,16*2,11*2,2*2,13*2,1*2
	dc.w	10*2,15*2,10*2,4*2,9*2,0*2,15*2,7*2
	dc.w	18*2,18*2,3*2,18*2,1*2,8*2,0*2,12*2
	dc.w	10*2,0*2,1*2,14*2,13*2,4*2,1*2,10*2
	dc.w	11*2,15*2,10*2,18*2,14*2,10*2,18*2,18*2
	dc.w	7*2,3*2,5*2,5*2,17*2,0*2,9*2,5*2
.xoffsetsend


*------	META BALLS EFFECT -----------------------------------------------------*

bufferw	equ	64
bufferh	equ	28
canvasw	equ	30
canvash	equ	bufferh
lineprefixsize	equ	4
linepostfixsize	equ	4+4 ; 0180 0000 xxdf fffe

metaballseffect
	move.l	v_dbclist1b(a5),a2		;
	bsr	scrolltext			; init bitplanes

	lea	v_buffer(a5),a1			;
	bsr	waitblitter			;
	move.w	#bufferw-canvasw,$66(a6)	; modulo D
	move.l	a1,$54(a6) 			; destination D
	move.l	#$01000000,$40(a6)		; bltcon0 / bltcon1
	move.w	#(bufferh<<6)+(canvasw>>1),$58(a6) ; bltsize and start
	bsr	waitblitter			;

	lea	sintab(pc),a2			; custom sinetable

	move.w	v_aball1(a5),d0			; ball 1
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	add.w	#2*11,v_aball1(a5)		;
	lea	v_buffer(a5),a1			;
	add.w	d0,a1				;
	move.w	v_bball1(a5),d0			;
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	asl.w	#6,d0				;
	add.w	d0,a1				;
	add.w	#2*9,v_bball1(a5)		;
	sub.w	#2*bufferw+1,a1			; adjust -> move 2 rows up, x-1
	bsr	drawball			;

	move.w	v_aball2(a5),d0			; ball 2
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	add.w	#2*12,v_aball2(a5)		;
	lea	v_buffer(a5),a1			;
	add.w	d0,a1				;
	move.w	v_bball2(a5),d0			;
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	asl.w	#6,d0				;
	add.w	d0,a1				;
	add.w	#2*11,v_bball2(a5)		;
	sub.w	#2*bufferw+1,a1			; adjust -> move 2 rows up x-1
	bsr	drawball			;

	move.w	v_aball3(a5),d0			; ball 3
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	add.w	#2*9,v_aball3(a5)		;
	lea	v_buffer(a5),a1			;
	add.w	d0,a1				;
	move.w	v_bball3(a5),d0			;
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	asl.w	#6,d0				;
	add.w	d0,a1				;
	add.w	#2*14,v_bball3(a5)		;
	sub.w	#2*bufferw+1,a1			; adjust -> move 2 rows up x-1
	bsr	drawball			;

	move.w	v_aball4(a5),d0			; ball 4
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	add.w	#2*10,v_aball4(a5)		;
	lea	v_buffer(a5),a1			;
	add.w	d0,a1				;
	move.w	v_bball4(a5),d0			;
	and.w	#$07fe,d0			;
	move.w	(a2,d0.w),d0			;
	asl.w	#6,d0				;
	add.w	d0,a1				;
	add.w	#2*13,v_bball4(a5)		;
	sub.w	#2*bufferw+1,a1			; adjust -> move 2 rows up x-1
	bsr	drawball			;

	move.l	v_dbclist1b(a5),a1		;
	add.w	#clist1size+lineprefixsize+2,a1	; +2 = skip $0180

	lea	v_buffer(a5),a2			;
	lea	bcols(pc),a0			;

	sub.w	v_mbcolsoffset(a5),a0		;

	moveq	#0,d0				;

	moveq	#canvash-1,d6			;
.loopy	moveq	#canvasw-1,d7			;
.loopx	move.b	(a2)+,d0			;
	move.w	(a0,d0.w),d1			;
	move.w	d1,0*onelinesize(a1)		; line 0
	move.w	d1,1*onelinesize(a1)		;
	move.w	d1,2*onelinesize(a1)		;
	move.w	d1,3*onelinesize(a1)		;
	move.w	d1,4*onelinesize(a1)		;
	move.w	d1,5*onelinesize(a1)		;
	move.w	d1,6*onelinesize(a1)		;
	move.w	d1,7*onelinesize(a1)		;
	addq.w	#4,a1				; next destination pixel x
;	addq.w	#2,d0				; next source pixel
	dbf	d7,.loopx			;

	add.w	#bufferw-canvasw,a2		;

	add.w	#8*onelinesize-(4*canvasw),a1	;
	dbf	d6,.loopy			;

	CHECKACTOR actor_metaballsshow
	beq	.noshow
	subq.w	#2,v_mbcolsoffset(a5)		;
	bne	.notdoneshow			;
	STOPACTOR actor_metaballsshow
.notdoneshow
.noshow
	CHECKACTOR actor_metaballshide
	beq	.nohide				;
	addq.w	#2,v_mbcolsoffset(a5)		;
	cmp.w	#60*2,v_mbcolsoffset(a5)	;
	bne	.notdonehide			;
	STOPACTOR actor_metaballshide
.notdonehide
.nohide

	movem.l	v_dbclist1a(a5),d0/d1		; clist1 double buffering
	exg	d0,d1				; v_dbclist1a <-> v_dbclist1b
	movem.l	d0/d1,v_dbclist1a(a5)		;
	rts					;

	rept	60
	dc.w	$0000				; fading
	endr
bcols	dc.w	$0000,$0111,$0222,$0333
	dc.w	$0444,$0555,$0666,$0777
	dc.w	$0888,$0999,$0aaa,$0bbb
	dc.w	$0ccc,$0ddd,$0eee,$0fff
	rept 12
	dc.w	$0fff,$0fff,$0fff,$0fff
	endr

onelinesize	equ	4*canvasw+lineprefixsize+linepostfixsize
wordoffset	equ	2
ballw		equ	17

bdata	dc.w	bufferw-ballw
	dc.w	7*wordoffset
	dc.w	13*wordoffset
	dc.w	15*wordoffset
	dc.w	9*wordoffset
	dc.w	10*wordoffset
	dc.w	11*wordoffset
	dc.w	14*wordoffset

drawball
	movem.w	bdata(pc),d0-d7			;
	addq.w	#1,a1				; row 1 (pixel not set)
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.w	#1,a1				; (pixel not set)
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 2
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 3
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	#5*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 4
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	#6*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d1,(a1)+			;
	add.b	d1,(a1)+			;
	add.b	#6*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 5
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	add.b	#5*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d1,(a1)+			;
	add.b	#5*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 6
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d1,(a1)+			;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 7
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	add.b	#6*wordoffset,(a1)+		;
	add.b	d4,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	#13*wordoffset,(a1)+		;
	add.b	d7,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	#6*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 8
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d1,(a1)+			;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 9
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d1,(a1)+			;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 8 = 10
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d1,(a1)+			;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 7 = 11
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	add.b	#6*wordoffset,(a1)+		;
	add.b	d4,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d3,(a1)+			;
	add.b	d7,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	#6*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 6 = 12
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d2,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d1,(a1)+			;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 5 = 13
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	add.b	#5*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d6,(a1)+			;
	add.b	d5,(a1)+			;
	add.b	d4,(a1)+			;
	add.b	d1,(a1)+			;
	add.b	#5*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 4 = 14
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	#6*wordoffset,(a1)+		;
	add.b	d1,(a1)+			;
	add.b	d1,(a1)+			;
	add.b	d1,(a1)+			;
	add.b	#6*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 3 = 15
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	add.b	#5*wordoffset,(a1)+		;
	addq.b	#4*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.b	#1*wordoffset,(a1)+		; row 2 = 16
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#3*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	add.w	d0,a1				;

	addq.w	#1,a1				; row 1 = 17 (pixel not set)
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#2*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)+		;
	addq.b	#1*wordoffset,(a1)		;
	rts					;


*------	SCROLLTEXT ------------------------------------------------------------*

; a2: taget copperlist
scrolltext
	moveq	#0,d0				;
	move.w	v_scrollpos(a5),d0		;
	move.l	d0,d2				; used later for 2nd plane

	move.l	d0,d1				;
	asr.l	#3,d1				;
	add.l	b_scrollplane(pc),d1		;

	move.l	v_bpl1pt(a5),a0			;
	add.l	a2,a0				;
	move.w	d1,4(a0)			;
	swap	d1				;
	move.w	d1,(a0)				;	

	subq.l	#taxishadowshift,d2		;
	move.l	d2,d1				;
	asr.l	#3,d1				;
	add.l	b_scrollplane(pc),d1		;
	sub.l	#shadow*scrollpwidth,d1		;

	move.l	v_bpl2pt(a5),a0			;
	move.w	d1,4(a2,a0)			;
	swap	d1				;
	move.w	d1,(a2,a0)			;

	not.w	d0				; flip
	and.w	#$000f,d0			;
	not.w	d2				; flip
	and.w	#$000f,d2			;
	asl.w	#4,d2				;
	or.w	d2,d0				;	
	move.l	v_bplcon1(a5),a0		;
	move.b	d0,1(a2,a0)			;

	tst.b	v_scroll(a5)			;
	beq	.noscroll			;
	addq.w	#scrollspeed,v_scrollpos(a5)	;
.noscroll

	move.w	v_textcolindex(a5),d0		; color cycling
	move.l	v_textcolor(a5),a0		;
	lea	.cols(pc),a1			;
	move.w	(a1,d0.w),(a2,a0)		; set color in clist4

	btst	#0,v_frame+1(a5)		; only any other frame
	beq	.1				;
	addq.w	#2,v_textcolindex(a5)		; next color
	cmp.w	#.colsend-.cols,v_textcolindex(a5) ;
	bne	.1				;
	clr.w	v_textcolindex(a5)		;
.1	rts					;

.cols	dc.w	$055a
	dc.w	$065b
	dc.w	$076c
	dc.w	$086d
	dc.w	$096d
	dc.w	$097d
	dc.w	$0a7d
	dc.w	$0a8d
	dc.w	$0b8d
	dc.w	$0b9d
	dc.w	$0a9d
	dc.w	$0aad
	dc.w	$0bad
	dc.w	$0bbd
	dc.w	$0cbd
	dc.w	$0ccd
	dc.w	$0dcd
	dc.w	$0ddd
	dc.w	$0cdd
	dc.w	$0bdd
	dc.w	$0acd
	dc.w	$08cd
	dc.w	$07bd
	dc.w	$06bd
	dc.w	$05bd
	dc.w	$05ac
	dc.w	$059c
	dc.w	$058c
	dc.w	$057b
	dc.w	$057a
	dc.w	$056a
.colsend


*------	TAXI EFFECT -----------------------------------------------------------*

taxieffect
	move.l	b_clist3(pc),a1			;
	add.w	#clist3bpl-clist3+2,a1		;
	move.l	v_db1a2a(a5),d0			; active bitplane
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	
	swap	d0				;
	sub.l	#taxishadowshift*pwidth,d0	;
	addq.w	#8,a1				;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	
	bsr	taxicolors			;
	bsr	cls				;

	if 0
	move.l	v_db1b2b(a5),a0			; test pattern top
	moveq	#-1,d0				;
	rept	10
	move.l	d0,(a0)+			;
	endr
	
	move.l	v_db1b2b(a5),a0			; test pattern bottom
	add.w	#255*pwidth,a0			;
	rept	10
	move.l	d0,(a0)+			;
	endr
	endif

	moveq	#pwidth-(cbwidth+2),d0		;
	move.w	d0,$62(a6)			; modulo B
	move.w	#-2,$64(a6)			; modulo A
	move.w	d0,$66(a6)			; modulo D

	moveq	#-1,d0				;
	clr.w	d0				;
	move.l	d0,$44(a6)			; first word mask, last word mask
	move.w	d0,$42(a6)			; bltcon1

	lea	$02(a6),a3			; $dff002
	lea	$58(a6),a6			; $dff058

	move.l	b_sintab(pc),a0			;
	move.w	v_taxia(a5),d2			;
	and.w	#$07fe,d2			;
	move.w	(a0,d2.w),d3			; sin angle
	add.w	#256+2*20,d3			; (2*20 = centering)
	asr.w	#1,d3				; x pos

	move.w	v_taxib(a5),d2			;
	and.w	#$07fe,d2			;
	add.w	#512,a0				;
	move.w	(a0,d2.w),d4			; cos angle
	add.w	#256-2*8,d4			; (2*8 = centering)
	asr.w	#1,d4				; y pos

	move.l	b_sintab(pc),a0			;
	move.w	v_taxic(a5),d2			;
	and.w	#$07fe,d2			;
	move.w	(a0,d2.w),d0			; sin angle
	add.w	#256+2*20,d0			; (2*20 = centering)
	asr.w	#1,d0				; x pos

	add.w	d0,d3				;
	asr.w	#1,d3				;

	move.w	v_taxid(a5),d2			;
	and.w	#$07fe,d2			;
	add.w	#512,a0				;
	move.w	(a0,d2.w),d0			; cos angle
	add.w	#256-2*8,d0			; (2*8 = centering)
	asr.w	#1,d0				; y pos

	add.w	d0,d4				;
	asr.w	#1,d4				;

	move.w	d4,v_taxicurrenty(a5)		;
	movem.w	d3/d4,v_taxix(a5)		;

	lea	v_taxidata(a5),a4		;
	moveq	#taxirows*taxicolumns-1,d7	;
.loopchars
	movem.w	v_taxix(a5),d3/d4		; "taxi" x y
	movem.w	(a4)+,d1/d2			; char x y
	move.l	(a4)+,a0			; char's base address
	
	sub.w	d1,d3				; dx = |x1 - x2|
	bpl	.xispositive			;
	neg.w	d3				;	
.xispositive
	sub.w	d2,d4				; dy = |y1 - y2|
	bpl	.yispositive			;
	neg.w	d4				;
.yispositive
	add.w	d3,d4				; d (taxicab/Manhattan distance) = dx + dy

	cmp.w	#71,d4				;
	bgt	.outofrange			;

	if testing
	movem.w	v_taxix(a5),d1/d2		; "taxi" x y
	endif

	; 32 (<<5) + 64 (<<6) = 96
	; LUT? No need - plenty of system time left
	asl.w	#5,d4				; * 96 (charheight*cbwidth)
	move.w	d4,d3				;
	add.w	d4,d4				;
	add.w	d4,d3				;
	
	add.l	d3,a0				;
	
.outofrange
	move.w	#%1111110000001101,d6		; word is swapped   $fc0d
	move.l	d1,d3				; d1 = xpos
	asl.l	#4,d3				;
	or.b	d3,d6				;
	ror.w	#8,d6				;
	asr.l	#3,d1				; calc word pos
	add.l	v_db1b2b(a5),d1			;
	
	asl.l	#3,d2				; = muls #pwidth,d2
	move.l	d2,d4				; 
	asl.l	#2,d4				;
	add.l	d4,d2				;

	add.l	d2,d1				;
	move.l	d1,a1				; copy for movem.l below

cuttop		equ	3
cutbottom	equ	3

	btst	#14-8,(a3)			; DMAB_BLTDONE = 14
.wait	btst	#14-8,(a3)			;
	bne	.wait				;

	move.w	d6,$40-$58(a6)			; bltcon0
	movem.l	d1/a0/a1,$4c-$58(a6)		; source B $4c, source A $50, destination D $54
	move.w	#(24-cuttop-cutbottom)<<6+(cbwidth+2)>>1,$58-$58(a6) ;
	dbf	d7,.loopchars			;
	
	lea	-$58(a6),a6			; a6 becomes $dff000 again

	movem.w	v_taxideltas(a5),d0-d3		;
	add.w	d0,v_taxia(a5)			;
	add.w	d1,v_taxib(a5)			;
	add.w	d2,v_taxic(a5)			;
	add.w	d3,v_taxid(a5)			;
	rts					;


*------	THE LORD/SPREADPOINT OF THE RINGS EFFECT ------------------------------*

chunk	equ	6

ringseffect
	move.l	b_clist4(pc),a1			;
	add.w	#clist4bpl3-clist4+2,a1		;
	move.l	v_db1a2a(a5),d0			; active bitplanes
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;

	bsr	colors				; note: must happen before scanline $10
	bsr	cls				;

	rem
	
	move.l	v_db1b2b(a5),a0			; test pattern top
	moveq	#-1,d0				;
	rept	10
	move.l	d0,(a0)+			;
	endr
	
	move.l	v_db1b2b(a5),a0			; test pattern bottom
	add.w	#255*pwidth,a0			;
	rept	10
	move.l	d0,(a0)+			;
	endr

	erem

	moveq	#pwidth-(cbwidth+2),d0		;
	move.w	d0,$62(a6)			; modulo B
	move.w	#-2,$64(a6)			; modulo A
	move.w	d0,$66(a6)			; modulo D

	moveq	#-1,d0				;
	clr.w	d0				;
	move.l	d0,$44(a6)			; first word mask, last word mask
	move.w	d0,$42(a6)			; bltcon1

	lea	$58(a6),a6			; a6 = $dff058

	lea	v_rings(a5),a3			;
.looprings
	move.b	ring_id(a3),d0			;
	btst	d0,v_setrings(a5)		; draw this ring?
	beq	.nextring			;

	move.l	ring_datap(a3),a2		;
	movem.w	ring_range(a3),d0/d5/d7		; ring_range, ring_index and ring_numchars

	lea	charoffsets(pc),a4		;
.loopchars
	cmp.w	d5,d0				;
	bgt	.inrange			;
	sub.w	d0,d5				;
.inrange
	move.l	(a4)+,a0			; char offset
	movem.w	(a2,d5.w),d1-d3			; x, y, angle
;	cmp.w	#1994,d1			; hint: clipping -> not used anymore
;	beq	.dontdraw			;

	add.l	v_adjcharbuffer(a5),a0		;
	add.l	d3,a0				;

	move.w	#%1111110000001101,d6		; word is swapped   $fc0d
	move.l	d1,d3				; d1 = xpos
	asl.l	#4,d3				;
	or.b	d3,d6				;
	ror.w	#8,d6				;
	asr.l	#3,d1				; calculate word pos
	add.l	v_adjdb1b2b(a5),d1		;
	add.l	d2,d1				;
	move.l	d1,a1				; copy for movem.l below

	btst	#14-8,$02-$58(a6)		; DMAB_BLTDONE = 14
.wait	btst	#14-8,$02-$58(a6)		;
	bne	.wait				;

	move.w	d6,$40-$58(a6)			; bltcon0
	movem.l	d1/a0/a1,$4c-$58(a6)		; source B $4c, source A $50, destination D $54
	move.w	#(24-cuttop-cutbottom)<<6+(cbwidth+2)>>1,$58-$58(a6) ;
	
.dontdraw
	add.w	ring_delta(a3),d5		; position/angle of next char
	dbf	d7,.loopchars			;

	move.w	ring_advance(a3),d7		;
	add.w	d7,ring_index(a3)		;

	cmp.w	ring_index(a3),d0		;
	bgt	.inrange2			;
	sub.w	d0,ring_index(a3)		;
.inrange2
.nextring
	add.w	#sizeofring,a3			; next ring
	lea	v_rings(a5),a4			;
	add.w	#numrings*sizeofring,a4		; done?
	cmp.l	a4,a3				;
	bne	.looprings			;

	lea	-$58(a6),a6			; a6 becomes $dff000 again
	rts					;


*------ TAXI COLORS -----------------------------------------------------------*

taxicolors
	move.w	v_taxicurrenty(a5),d0		;
	sub.w	#$2f,d0				; $0..$cf  207

	move.l	b_clist3(pc),a1			; taxi color
	add.w	#clist3rows+10-clist3,a1	; -> value for $0182
	
	lea	v_taxicollut(a5),a0		;
	moveq	#0,d1				;
	move.b	(a0,d0.w),d1			;
	add.w	d1,d1				; word offset
	lea	.cols-4(pc,d1.w),a0		;
	
	moveq	#taxirows-1,d7			;
.loop	move.w	(a0)+,(a1)			; set color in clist4
	add.w	#12,a1				; skip copper instructions
	dbf	d7,.loop			;
	rts					;
	
.cols	dc.w	$077d
	dc.w	$087d
	dc.w	$097d
	dc.w	$0a8d
	dc.w	$0b8d
	dc.w	$0bad
	dc.w	$0ccd
	dc.w	$0ddd
	dc.w	$0cdd
	dc.w	$0bdd
	dc.w	$0acd
	dc.w	$08cd
	dc.w	$07bd
	dc.w	$06bd
	dc.w	$05bd
	dc.w	$04bd
	dc.w	$03bd
.colsend


*------	RING COLORS -----------------------------------------------------------*

;clist4ringcols
;	dc.w	$0190,0		; ring 6
;	dc.w	$0192,0		; ring 5
;	dc.w	$0194,0		; ring 4
;	dc.w	$0198,0		; ring 3
;	dc.w	$0196,0		; ring 2
;	dc.w	$019e,0 	; ring 1

colors	move.w	v_ringscolindex(a5),d0		;
	move.l	b_clist4(pc),a1			; ring colors
	add.w	#clist4ringcols+2-clist4,a1	;
	moveq	#numrings-1,d7			;
.loop	move.w	.rcols(pc,d0.w),(a1)		; set color in clist4
	addq.w	#4,a1				;
	addq.w	#2,d0				; next color
	cmp.w	#.rcolsend-.rcols,d0		;
	bne	.1				;
	moveq	#0,d0				;
.1	dbf	d7,.loop			;

	btst	#0,v_frame+1(a5)		; only any other frame
	beq	.2				;
	addq.w	#2,v_ringscolindex(a5)		; next color
	cmp.w	#.rcolsend-.rcols,v_ringscolindex(a5) ;
	bne	.2				;
	clr.w	v_ringscolindex(a5)		;
.2	rts					;

.rcols	dc.w	$0136
	dc.w	$0237
	dc.w	$0338
	dc.w	$0449
	dc.w	$055a
	dc.w	$065b
	dc.w	$076c
	dc.w	$086d
	dc.w	$096d
	dc.w	$097d
	dc.w	$0a7d
	dc.w	$0a8d
	dc.w	$0b8d
	dc.w	$0b9d
	dc.w	$0a9d
	dc.w	$0aad
	dc.w	$0bad
	dc.w	$0bbd
	dc.w	$0cbd
	dc.w	$0ccd
	dc.w	$0dcd
	dc.w	$0ddd
	dc.w	$0cdd
	dc.w	$0bdd
	dc.w	$0acd
	dc.w	$08cd
	dc.w	$07bd
	dc.w	$06bd
	dc.w	$05bd
	dc.w	$04ac
	dc.w	$039b
	dc.w	$038a
	dc.w	$0279
	dc.w	$0268
	dc.w	$0157
.rcolsend


*------	GENERATE RINGS --------------------------------------------------------*

numringplanes	equ	3
ringplaneheight	equ	pheight

generaterings
	move.l	b_ringplanes(pc),a0		;
	lea	plan1(pc),a2			;
	bsr	gencircles			; (saves a0)
	tst.b	v_doquit(a5)			;
	bne	.quit				;
	bsr	mirrorandcopy			; a0 = source/dest

	move.l	b_ringplanes(pc),a0		;
	add.w	#1*ringplaneheight*pwidth,a0	;
	lea	plan2(pc),a2			;
	bsr	gencircles			; (saves a0)
	tst.b	v_doquit(a5)			;
	bne	.quit				;
	bsr	mirrorandcopy			; a0 = source/dest

	move.l	b_ringplanes(pc),a0		;
	add.w	#2*ringplaneheight*pwidth,a0	;
	lea	plan3(pc),a2			;
	bsr	gencircles			; (saves) a0
	tst.b	v_doquit(a5)			;
	bne	.quit				;
	bsr	mirrorandcopy			; a0 = source/dest (faster: bra mirrorandcopy)
.quit	rts					;

; a0: bitplane a2: plan
gencircles
	move.l	a0,-(a7)			;
	move.w	#-ringplaneheight/2,d1		; y
	ext.l	d1				;
	moveq	#7,d4				; bit to set
.yloop	move.l	#-160,d0			; x
.xloop	move.l	d0,d2				; copy of x
	muls	d2,d2				; x² (hint: muls LUT isn't faster)
	move.l	d1,d3				; copy of y
	muls	d3,d3				; y²
	add.l	d2,d3				; x² + y²

	move.l	a2,a1				;
	move.w	(a1)+,d7			; num rings
.rings	movem.l	(a1)+,d5/d6			;
	cmp.l	d5,d3				; x² + y² <= r²
	bge	.next				;
	cmp.l	d6,d3				;
	blt	.next				;
	bset	d4,(a0)				;
	bra	.done				;
.next	dbf	d7,.rings			;
	
.done	addq.l	#1,d0				; advance x
	subq.b	#1,d4				;
	bpl	.stay				;
	moveq	#7,d4				; advance to next byte
	addq.l	#1,a0				;
.stay	tst.w	d0				; half x?
	bne	.xloop				;

	add.w	#pwidth/2,a0			; skip upper right quadrant

	tst.b	v_doquit(a5)			;
	bne	.quit				;

	addq.l	#1,d1				; advance y
	bne	.yloop				; until y=0 (half)
.quit	move.l	(a7)+,a0			;
	rts					;

plan1	dc.w	(p1end-p1)/8-1
p1	dc.l	(128+10)*(128+10),(108+10)*(108+10)
	dc.l	(108+10)*(108+10),(88+12)*(88+12)
;	dc.l	(88+12)*(88+12),(68+14)*(68+14)
;	dc.l	(68+14)*(68+14),(48+14)*(48+14)
	dc.l	(48+14)*(48+14),(28+16)*(28+16)
p1end

plan2	dc.w	(p2end-p2)/8-1
p2	dc.l	(128+10)*(128+10),(108+10)*(108+10)
	dc.l	(108+10)*(108+10),(88+12)*(88+12)
;	dc.l	(88+12)*(88+12),(68+14)*(68+14)
	dc.l	(68+14)*(68+14),(48+14)*(48+14)
;	dc.l	(48+14)*(48+14),(28+16)*(28+16)
p2end

plan3	dc.w	(p3end-p3)/8-1
p3	dc.l	(128+10)*(128+10),(108+10)*(108+10)
;	dc.l	(108+10)*(108+10),(88+12)*(88+12)
	dc.l	(88+12)*(88+12),(68+14)*(68+14)
;	dc.l	(68+14)*(68+14),(48+14)*(48+14)
;	dc.l	(48+14)*(48+14),(28+16)*(28+16)
p3end

; a0: source/destination
mirrorandcopy
	move.l	a0,a1				;
	moveq	#0,d0				;
	move.w	#ringplaneheight/2-1,d7		; height
.loop
index set pwidth-1
	rept	pwidth/2
	move.b	(a0)+,d0			;
	move.b	v_mirror(a5,d0.w),index(a1)	;
index set index-1 
	endr
	add.w	#pwidth/2,a0			;
	add.w	#pwidth,a1			;
	dbf	d7,.loop			;

	move.l	a0,a1				; mirror upper hald
	move.w	#ringplaneheight/2-1,d7		; height
.copy	sub.w	#pwidth,a0			;
	moveq	#pwidth/4-1,d6			;
.line	move.l	(a0)+,(a1)+			;
	dbf	d6,.line			;
	sub.w	#pwidth,a0			;

	if testing
	move.w	d7,$dff180			;
	endif
	dbf	d7,.copy			;
	rts					;


*------ SET SPRITE POINTERS ---------------------------------------------------*

; a0: clist d0: sprite data
setspritepointers
	moveq	#8-1,d7				; 8 sprite pointers
.loop	swap	d0				; sprite0
	move.w	d0,(a0)				;
	swap	d0				;
	move.w	d0,4(a0)			;
	addq.l	#8,a0				; next pointer
	dbf	d7,.loop			;
	rts					;


*------ SPRITE DATA -----------------------------------------------------------*

spritedata
	dc.w	$1905,$1a00	; 1px high sprite in the top-leftmost valid position
	dc.w	$0000,$0000	; blank pixel data
	dc.w	$0000,$0000	; end of sprite
spritedataend


*------	PLAYER ----------------------------------------------------------------*

cmdoffset	equ	2		; word offset (cmds jump table)

cmd_wait	equ 	0*cmdoffset
cmd_actor_start	equ	1*cmdoffset
cmd_actor_stop	equ	2*cmdoffset
cmd_metaballs	equ	3*cmdoffset
cmd_setrings	equ	4*cmdoffset
cmd_setvar	equ	5*cmdoffset
cmd_logo	equ	6*cmdoffset
cmd_taxi	equ	7*cmdoffset
cmd_quit	equ	8*cmdoffset
cmd_scrollpos	equ	9*cmdoffset
cmd_rings	equ	10*cmdoffset
cmd_scroll	equ	11*cmdoffset

cmds	dc.w	0 ; cmd_wait (must be zero)
	dc.w	cmdactorstart-cmds
	dc.w	cmdactorstop-cmds
	dc.w	cmdmetaballs-cmds
	dc.w	cmdsetrings-cmds
	dc.w	cmdsetvar-cmds
	dc.w	cmdlogo-cmds
	dc.w	cmdtaxi-cmds
	dc.w	cmdquit-cmds
	dc.w	cmdscrollpos-cmds
	dc.w	cmdrings-cmds
	dc.w	cmdscroll-cmds

play	tst.b	v_doquit(a5)		;
	bne	.quit			; don't mess up with precalc
	tst.b	v_wait(a5)		;
	beq	.donotwait		;
	subq.b	#1,v_wait(a5)		;
.quit	rts				;
.donotwait
	move.l	v_cmdspointer(a5),a0	;
loop	move.b	(a0)+,d0		;
	bne	.notcmdwait		;
	move.b	(a0)+,v_wait(a5)	; cmd_wait duration
	move.l	a0,v_cmdspointer(a5)	;
	if numbers
	addq.b	#1,v_waitcount(a5)	; sync helper
	endif
	rts				;

.notcmdwait
	ext.w	d0			;
	lea	cmds(pc),a1		;
	add.w	(a1,d0.w),a1		;
	jsr	(a1)			; execute cmd
	bra	loop			;
	
cmdactorstart
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		;
	bset	d1,d2			;
	bra	set			;

cmdactorstop
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		;
	bclr	d1,d2			;
set	move.l	d2,v_actors(a5)		;
	rts				;

cmdmetaballs
	if framelaps
	clr.w	v_framelap(a5)		;
	endif

	move.l	v_dbclist1b(a5),$80(a6)	; hmmm... strange vs v_dbclist1a(a5)
	STARTACTOR actor_metaballs	;
	rts				;

cmdsetrings
	move.b	(a0)+,v_setrings(a5)	;
	rts				;

cmdsetvar
	moveq	#0,d1			;
	move.b	(a0)+,d1		; value (upper byte)
	asl.w	#8,d1			;
	move.b	(a0)+,d1		; value (lower byte)

	moveq	#0,d2			;
	move.b	(a0)+,d2		; var (upper byte)
	asl.w	#8,d2			;
	move.b	(a0)+,d2		; var (lower byte)

	move.w	d1,(a5,d2.w)		; set var
	rts				;

cmdscrollpos
	move.b	(a0)+,d1		; value (upper byte)
	asl.w	#8,d1			;
	move.b	(a0)+,d1		; value (lower byte)
	move.w	d1,v_scrollpos(a5)	;
	rts				;

cmdlogo	
	if framelaps
	clr.w	v_framelap(a5)		;
	endif

	move.l	b_clist5(pc),$80(a6)	;
	STARTACTOR actor_logo		;
	rts

cmdtaxi
	if framelaps
	clr.w	v_framelap(a5)		;
	endif

	move.l	b_clist3(pc),$80(a6)	;
	STARTACTOR actor_taxi		;
	rts				;

cmdquit	st	v_doquit(a5)		;
	rts				;

cmdrings
	if framelaps
	clr.w	v_framelap(a5)		;
	endif

	move.l	b_clist4(pc),$80(a6)	;
	STARTACTOR actor_rings		;
	rts				;

cmdscroll
	move.b	(a0)+,v_scroll(a5)	;
	rts				;

playcmds
	dc.b	cmd_actor_start,actor_prepare_demo

	dc.b	cmd_wait,50	; 1 sec of silence
	dc.b	cmd_actor_start,actor_logo_fading

	dc.b	cmd_wait,160
	dc.b	cmd_scroll,1	; START

	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,55

	dc.b	cmd_scroll,0	; SPREADPOINTS

	dc.b	cmd_wait,30

	dc.b	cmd_actor_start,actor_logo_fading
	dc.b	cmd_wait,50+64+10

	dc.b	cmd_actor_stop,actor_logo
	dc.b	cmd_metaballs

;	dc.b	cmd_wait,0,cmd_setvar,59*2>>8,(59*2)&$ff,v_mbcolsoffset>>8,v_mbcolsoffset&$ff

	dc.b	cmd_actor_start,actor_metaballsshow
	dc.b	cmd_actor_start,actor_clear_bitplanes

;	dc.b	cmd_scroll,1

	dc.b	cmd_wait,200
	dc.b	cmd_scroll,1
	dc.b	cmd_wait,100

	dc.b	cmd_wait,200
	dc.b	cmd_wait,250
	dc.b	cmd_wait,150
	dc.b	cmd_scroll,0
	dc.b	cmd_wait,100

	dc.b	cmd_actor_start,actor_metaballshide
	dc.b	cmd_wait,60

	dc.b	cmd_actor_stop,actor_metaballs

	dc.b	cmd_taxi
	dc.b	cmd_actor_start,actor_prepare_txt ; greetingslist
	
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250-64-10
	dc.b	cmd_wait,250
	dc.b	cmd_wait,40

	dc.b	cmd_actor_stop,actor_taxi

	dc.b	cmd_actor_start,actor_metaballsshow
	dc.b	cmd_scrollpos,$00,$0b ; prevent from showing trash
	dc.b	cmd_actor_start,actor_clear_bitplanes ; prepare rings

	dc.b	cmd_metaballs
	dc.b	cmd_wait,100
	dc.b	cmd_scroll,1
	dc.b	cmd_wait,150
	dc.b	cmd_wait,250

	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_scroll,0	; hide trash in bitplane
	dc.b	cmd_wait,147

	dc.b	cmd_actor_start,actor_metaballshide
	dc.b	cmd_wait,60
	dc.b	cmd_actor_stop,actor_metaballs
	dc.b	cmd_rings

wait1	equ	0
	dc.b	cmd_wait,wait1
	dc.b	cmd_setrings,%100000
	dc.b	cmd_wait,250
	dc.b	cmd_setrings,%010000
	dc.b	cmd_wait,wait1
	dc.b	cmd_setrings,%001000
	dc.b	cmd_wait,wait1
	dc.b	cmd_setrings,%000100
	dc.b	cmd_wait,wait1
	dc.b	cmd_setrings,%000010
	dc.b	cmd_wait,wait1
	dc.b	cmd_setrings,%000001
	dc.b	cmd_wait,wait1

	dc.b	cmd_setrings,%000000
	dc.b	cmd_wait,90-32
wait2	equ	0
	dc.b	cmd_setrings,%000001
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%000011
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%000111
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%001111
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%011111
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%111111

	rept 3
	dc.b	cmd_wait,250
	endr
	dc.b	cmd_wait,200

	dc.b	cmd_setrings,%111110
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%111100
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%111000
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%110000
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%100000
	dc.b	cmd_wait,wait2
	dc.b	cmd_setrings,%000000

	dc.b	cmd_wait,40
	dc.b	cmd_quit
	dc.b	cmd_wait,255 ; you never know ;-)

	even


*------	COPPER INSTRUCTION LIST 1 META BALLS ----------------------------------*

clist1	
clist1cop
	dc.w	$0080,0
	dc.w	$0082,0
	dc.w	$1007,$fffe ; chance for player to alter clist in time

spritepointersclist1
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
;	dc.w	$0092,$0038
	dc.w	$0092,$0030 ; wider because of scrolltext
	dc.w	$0094,$00d0

	dc.w	$0100,$0200

	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,scrollpwidth-pwidth-2	; -2 = scrolling
	dc.w	$010a,scrollpwidth-pwidth-2

	dc.w	$0180,$0000
	dc.w	$0182,$0000
clist1end


*------	COPPER INSTRUCTION LIST 3 TAXI ----------------------------------------*

clist3	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist3bpl
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0

spritepointersclist3
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$2600	; dual playfield mode
	dc.w	$0102,taxishadowshift<<4 ; shift shadow playfield
	dc.w	$0104,$0000

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0180,$0000
	dc.w	$0192,$0222	; shadow collor
clist3rows

row set $2c
	rept taxirows-1
	dc.w	$0190,$0000 ; nop
	dc.b	row&$ff,$07
	dc.w	$fffe,$0182,0 ; $0180 = testing
row set row+taxideltay
	endr

	dc.w	$ffdf,$fffe ; PAL/NTSC wait instead of nop
	dc.w	$1607,$fffe,$0182,0 ; $0180 = testing

	dc.w	$ffff,$fffe
clist3end


*------	COPPER INSTRUCTION LIST 4 THE LORD/SPREADPOINT OF THE RINGS -----------*

clist4	dc.w	$1007,$fffe ; chance to alter clist in time
clist4bpl
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0
clist4bpl3
	dc.w	$00ec,0,$00ee,0

spritepointersclist4
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e
	dc.b	$2c-8,$81
	dc.w	$0090
	dc.b	$2c+8,$c1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$4200
	dc.w	$0102,$0000

	dc.w	$0108,inviswidth
	dc.w	$010a,inviswidth

	if testing
	dc.w	$0180,$0000
	dc.w	$0182,$0800
	dc.w	$0184,$0f00
	dc.w	$0186,$0080
	dc.w	$0188,$00f0
	dc.w	$018a,$0008
	dc.w	$018c,$000f
	dc.w	$018e,$0f0f
	else
	dc.w	$0180,$0000
	dc.w	$0182,$0000
	dc.w	$0184,$0000
	dc.w	$0186,$0000
	dc.w	$0188,$0000
	dc.w	$018a,$0000
	dc.w	$018c,$0000
	dc.w	$018e,$0000
	endif

	dc.w	$019a,$0000
	dc.w	$019c,$0000
clist4ringcols
	dc.w	$019e,0 	; ring 1
	dc.w	$0196,0		; ring 2
	dc.w	$0198,0		; ring 3
	dc.w	$0194,0		; ring 4
	dc.w	$0192,0		; ring 5
	dc.w	$0190,0		; ring 6

	dc.w	$ffff,$fffe
clist4end


*------	COPPER INSTRUCTION LIST 5 LOGO ----------------------------------------*

clist5	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist5bpl
	dc.w	$00e8,0,$00ea,0 ; logo bitplane 1
	dc.w	$00ec,0,$00ee,0 ; logo bitplane 2

	dc.w	$00e0,0,$00e2,0 ; noise bitplane 1
	dc.w	$00e4,0,$00e6,0 ; noise bitplane 2

spritepointersclist5
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$0200
	dc.w	$0102,$0000
	dc.w	$0104,$0000

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0180,$0000
	dc.w	$0182,$0000
	dc.w	$0184,$0000
	dc.w	$0186,$0000

	dc.w	$018c,$0000 ; logo noise color (black)
clist5cols
	dc.w	$0188,0	; logo noise color
	dc.w	$018a,0	; logo noise color
	dc.w	$018e,0	; logo noise color
	; frame color registers (8)
	dc.w	$0190,0
	dc.w	$0192,0
	dc.w	$0194,0
	dc.w	$0196,0
	dc.w	$0198,0
	dc.w	$019a,0
	dc.w	$019c,0
	dc.w	$019e,0

logostart	equ	$46
	dc.b	logostart,$01 ; start of logo
	dc.w	$fffe
	dc.w	$0100,$4200 ; hide everything

	dc.b	logostart+logoheight,$01 ; end of logo
	dc.w	$fffe
	dc.w	$0100,$0200 ; hide everything

	dc.w	$ffdf,$fffe

	dc.w	$0092,$0030 ; wider because of scrolltext
	dc.w	$0108,scrollpwidth-pwidth-2	; -2 = scrolling
	dc.w	$010a,scrollpwidth-pwidth-2

	; do not alter clist from here
	dc.w	$0102
clist5marker
	dc.w	0
	dc.w	$00e0,0	
	dc.w	$00e2,0

	dc.w	$00e4,0
	dc.w	$00e6,0

	dc.w	spos<<8+$01,$fffe
	dc.w	$0100,$2600
	dc.w	$0182,0
	dc.w	$0192,$0000
	dc.w	(spos+shadow)<<8+$01,$fffe
	dc.w	$0192,$0333 ; shadow
	dc.w	(spos+visiblefontheight)<<8+$01,$fffe
	dc.w	$0182,$0000
	dc.w	$0104,1<<6
	dc.w	(spos+visiblefontheight+shadow)<<8+$01,$fffe
	dc.w	$0100,$0200
	dc.w	$ffff,$fffe
clist5end


*------	CLEAR SCREEN ----------------------------------------------------------*

cpuclslines	equ	130
cpuclschunk	equ	10	; 10 * 4 (longword) = 40 (=visible pwidth)

cls	move.l	v_db1b2b(a5),a4			;
	bsr	waitblitter			;

	move.w	#inviswidth,$66(a6)		; modulo D
	move.l	#$01000000,$40(a6)		; bltcon0 bltcon1
	move.l	a4,$54(a6)			; destination D
	move.w	#(pheight-cpuclslines)<<6+(pwidth-inviswidth)>>1,$58(a6) ; bltsize and start
	add.w	#psize,a4			; end of bitplane
	moveq	#(cpuclslines/cpuclschunk)-1,d7	; 14 * 10 lines
	movem.l	v_zero(a5),d0-d6/a0-a2		; 10 registers * 4 bytes = 40 (1 visible line)
.cpucls	
	rept cpuclschunk
;	subq.w	#inviswidth,a4			;
	movem.l	d0-d6/a0-a2,-(a4)		; clears 1 line (pwidth)
	endr
	dbf	d7,.cpucls			;

	if timing
	move.w	#$0fff,$180(a6)			; white should be as small as possible
	endif

	bsr	waitblitter			; improve: move away from here

	if timing
	move.w	#$0000,$180(a6)			;
	endif

	rts					;


*------	MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

clist1size	equ	clist1end-clist1
clist1extrasize	equ	(4*canvasw+4+4+4)*canvash*8+4+100; CHECK  *4 0180 +4 wait +4 wait +4 0180
clist3size	equ	clist3end-clist3
clist4size	equ	clist4end-clist4
clist5size	equ	clist5end-clist5
lspbanksize	equ	lspbankend-lspbank
spritedatasize	equ	spritedataend-spritedata
logosize	equ	logoend-logo

; note: MEMF_CLEAR for extra safety
memtable
b_clist3	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist3size
b_clist4	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist4size
b_clist5	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist5size
b_lspbank	dc.l	0,MEMF_CHIP+MEMF_CLEAR,lspbanksize
b_spritedata	dc.l	0,MEMF_CHIP+MEMF_CLEAR,spritedatasize
b_logo		dc.l	0,MEMF_CHIP+MEMF_CLEAR,logosize

memtable2
b_clist1a	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist1size+clist1extrasize
b_clist1b	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist1size+clist1extrasize
b_bitplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,bitplanessize
b_scrollplane	dc.l	0,MEMF_CHIP+MEMF_CLEAR,scrollpwidth*visiblefontheight
b_ringplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,numringplanes*ringplaneheight*pwidth
b_charbuffer	dc.l	0,MEMF_CHIP+MEMF_CLEAR,numchars*rotationsteps*charheight*cbwidth
b_vars		dc.l	0,MEMF_CHIP+MEMF_CLEAR,sizeofvars
b_sintab	dc.l	0,BESTMEMORY+MEMF_CLEAR,2048+512
;b_testoutofmem	dc.l	0,MEMF_CHIP,600000
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

allocandinit
	lea	clist1(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	btst	#1,d0				; chipmem?
	beq	.notchipmem			;

	lea	clist3(pc),a0			;
	lea	b_clist3(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist4(pc),a0			;
	lea	b_clist4(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist5(pc),a0			;
	lea	b_clist5(pc),a1			;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#lspbank-base,a0		;
	lea	b_lspbank(pc),a1		;
	move.l	a0,(a1)				;

	lea	spritedata(pc),a0		;
	lea	b_spritedata(pc),a1		;
	move.l	a0,(a1)				;

	lea	logo(pc),a0			;
	lea	b_logo(pc),a1			;
	move.l	a0,(a1)				;
.notchipmem
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			;
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		;
	jsr	AllocMem(a6)			;
	move.l	d0,(a5)				;
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			;
	bsr	init				;
	moveq	#0,d0				; ok, all entries allocated
	rts					;

.printerrorandfreemem
	bsr	printoutofmemory		;
dealloc	lea	clist1(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7	;
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				;
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr	FreeMem(a6)			;
	add.w	#entrysize,a5			;
	dbf	d7,.loop			;
.done	moveq	#-1,d0				; alloc error
	rts					;

init	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		;
	move.l	b_lspbank(pc),a1		;
	move.l	#lspbanksize,d0			;
.copylspbank
	move.b	(a0)+,(a1)+			;
	subq.l	#1,d0				;
	bne	.copylspbank			;

	move.l	b_vars(pc),a5			;

	move.l	b_bitplanes(pc),a0		;
	add.w	#invissize,a0			;
	move.l	a0,a2				;

	lea	v_db1a2a(a5),a1			;
	move.l	a0,(a1)+			; v_db1a2a
	add.w	#invissize+psize,a0		;
	move.l	a0,(a1)+			; v_db1b2b

	add.w	#center,a2			;
	move.l	a2,(a1)+			; v_adjdb1a2a
	add.w	#center,a0			;
	move.l	a0,(a1)				; v_adjdb1b2b

	move.l	b_spritedata(pc),d0		; important: do this before copy
	lea	clist1(pc),a0			; sprites clist 1
	add.w	#spritepointersclist1+2-clist1,a0 ;
	bsr	setspritepointers		;

	lea	clist1(pc),a0			; copy clist 1 to chip memory
	move.l	b_clist1a(pc),a1		;
	move.w	#clist1size-1,d7		;
.copyc1	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc1			;

	move.w	#$2c4f,d0			; $2c3f
	moveq	#-2,d1				; const value $fffffffe
	move.w	#canvash*8-1,d7			;
.gen	move.w	d0,(a1)+			; pos
	move.w	d1,(a1)+			; $fffe
	moveq	#canvasw+1-1,d6			; 40 + 1 "stop black non bleeding"
.gen2	
	if timing
	move.w	#$0182,(a1)+			; $0182 -> make time bar visible
	else
	move.w	#$0180,(a1)+			; $0180
	endif

;	move.w	d6,d5				; test color
;	add.w	d7,d5				;
;	move.w	d5,(a1)+			; testing

	clr.w	(a1)+				; black
	dbf	d6,.gen2			;

	move.w	d0,d2				; wait at end of line
	move.b	#$df,d2				;
	move.w	d2,(a1)+			;
	move.w	d1,(a1)+			;
	add.w	#$0100,d0			; next line
	dbf	d7,.gen				;

	; scroller
	move.w	#$0102,(a1)+			;
	move.l	a1,v_bplcon1(a5)		; set marker (crap - who cares)
	clr.w	(a1)+				;

	move.w	#$00e0,(a1)+			; bitplane 1
	move.l	a1,v_bpl1pt(a5)			; set marker
	clr.w	(a1)+				;
	move.l	#$00e20000,(a1)+		;

	move.w	#$00e4,(a1)+			; bitplane 2
	move.l	a1,v_bpl2pt(a5)			; set marker
	clr.w	(a1)+				;
	move.l	#$00e60000,(a1)+		;

visiblefontheight	equ	13
shadow	equ	4
spos	equ	$19
	move.l	#spos<<24+$01fffe,(a1)+		;
	move.l	#$01002600,(a1)+		; dual playfield mode
	move.w	#$0182,(a1)+			;
	move.l	a1,v_textcolor(a5)		; set marker
	clr.w	(a1)+				;

	move.l	#$01920000,(a1)+		; hide shadow garbage
	move.l	#(spos+shadow)<<24+$01fffe,(a1)+ ;
	move.l	#$01920333,(a1)+		; shadow color
	move.l	#(spos+visiblefontheight)<<24+$01fffe,(a1)+ ;
	move.l	#$01820000,(a1)+		; hide possible garbage
	move.l	#$0104<<16+1<<6,(a1)+		; gives playfield 2 priority over playfield 1
	move.l	#(spos+visiblefontheight+shadow)<<24+$01fffe,(a1)+ ;
	move.l	#$01000200,(a1)+		;
	move.l	d1,(a1)				; end of clist

	move.l	b_clist1a(pc),d0		; finalize markers
	sub.l	d0,v_bplcon1(a5)		;
	sub.l	d0,v_bpl1pt(a5)			;
	sub.l	d0,v_bpl2pt(a5)			;
	sub.l	d0,v_textcolor(a5)		;

	move.l	b_clist1a(pc),a0		; copy double buffered
	move.l	b_clist1b(pc),a1		;
	move.w	#(clist1size+clist1extrasize)/2-1,d7 ;
.copycb	move.w	(a0)+,(a1)+			;
	dbf	d7,.copycb			;

	move.l	b_clist1a(pc),a0		;
	move.l	b_clist1b(pc),a1		;
	
	move.l	a0,v_dbclist1a(a5)		;
	move.l	a1,v_dbclist1b(a5)		;
	
	move.l	a0,d0				;
	move.w	d0,clist1cop-clist1+2+4(a1)	;
	swap	d0				;
	move.w	d0,clist1cop-clist1+2(a1)	;

	move.l	a1,d0				;
	move.w	d0,clist1cop-clist1+2+4(a0)	;
	swap	d0				;
	move.w	d0,clist1cop-clist1+2(a0)	;

	move.l	v_dbclist1b(a5),a2		;
	bsr	scrolltext			; init bitplanes

	move.l	b_clist5(pc),a2			;
	sub.l	v_bplcon1(a5),a2		;
	add.l	#clist5marker-clist5,a2		;
	bsr	scrolltext			; init bitplanes

;	dc.w	$2c3f,$fffe ; 1 pixel too early - never mind
;	rept 10
;	dc.w	$0180,$0f00
;	dc.w	$0180,$00f0
;	dc.w	$0180,$044f
;	dc.w	$0180,$00ff
;	endr
;	dc.w	$0180,$0000
;	dc.w	$2cdf,$fffe

	lea	clist3(pc),a0			; copy clist 3 to chip memory
	move.l	b_clist3(pc),a1			;
	move.w	#clist3size-1,d7		;
.copyc3	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc3			;

	lea	clist4(pc),a0			; copy clist 4 to chip memory
	move.l	b_clist4(pc),a1			;
	move.w	#clist4size-1,d7		;
.copyc4	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc4			;

	lea	clist5(pc),a0			; copy clist 5 to chip memory
	move.l	b_clist5(pc),a1			;
	move.w	#clist5size-1,d7		;
.copyc5	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc5			;

	lea	logo(pc),a0			; copy logo to chip memory
	move.l	b_logo(pc),a1			;
	move.w	#logosize-1,d7			;
.copyl	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyl			;

	lea	spritedata(pc),a0		; copy sprite data to chip memory
	move.l	b_spritedata(pc),a1		;
	move.w	#spritedatasize-1,d7		;
.copysd	move.b	(a0)+,(a1)+			;
	dbf	d7,.copysd			;

	move.l	b_clist3(pc),a0			; sprites clist 3
	add.w	#spritepointersclist3+2-clist3,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist4(pc),a0			; sprites clist 4
	add.w	#spritepointersclist4+2-clist4,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist5(pc),a0			; sprites clist 5
	add.w	#spritepointersclist5+2-clist5,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist4(pc),a1			; bitplane pointers clist 4
	add.w	#clist4bpl+2-clist4,a1		;
	move.l	b_ringplanes(pc),a0		;
	moveq	#numringplanes-1,d7		; 4 ringsplanes
.initrp	move.l	a0,d0				;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	addq.w	#8,a1				;
	add.w	#ringplaneheight*pwidth,a0	; next ringplane
	dbf	d7,.initrp			;

	move.l	b_clist5(pc),a1			; bitplane pointers clist 5
	add.w	#clist5bpl+2-clist5,a1		;
	move.l	b_logo(pc),d0			;
	moveq	#2-1,d7				; 2 logo bitplanes
.initbpllogo
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	swap	d0				;
	addq.w	#8,a1				;
	add.l	#logosize/2,d0			; = logosize/2
	dbf	d7,.initbpllogo			;
	
	move.l	b_bitplanes(pc),d0		;
	move.l	d0,a0				;
	moveq	#2-1,d7				; 2 noise bitplanes
.initbplnoise
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	swap	d0				;
	addq.w	#8,a1				;
	add.l	#logopsize,d0			;
	dbf	d7,.initbplnoise		;

	move.w	#2*logopsize-1,d7		; generate initial noise
	moveq	#70,d0				;
	moveq	#45,d1				;
.noise	move.b	d0,(a0)+			;
	eor.w	d7,d0				; the magic sauce
	eor.w	d1,d0				;
	sub.w	d7,d0				;
	subq.w	#7,d1				;	
	ror.w	d0,d1				;
	dbf	d7,.noise			;

	bsr	generatesintab			; sintab

	moveq	#0,d7				; generate byte mirror
.mirror	moveq	#0,d6				; resulting mirrored byte value
	moveq	#7,d0				; highest bit
	moveq	#0,d1				; lowest bit
.miloop	btst	d0,d7				;
	beq	.nset				;
	bset	d1,d6				;
.nset	addq.b	#1,d1				;
	subq.b	#1,d0				;
	bpl	.miloop				;	
	move.b	d6,v_mirror(a5,d7.w)		;
	addq.b	#1,d7				; = addq.w #1,d7, cmp.w	#256,d7
	bne	.mirror				; not yet $00 (= $100)

	lea	playcmds(pc),a0			; init and start player
	move.l	a0,v_cmdspointer(a5)		;
	STARTACTOR actor_player			;

	bsr	inittaxieffect			;

	; could be optimized - who cares

	move.l	b_charbuffer(pc),a0		;
	add.w	#cuttop*cbwidth,a0		;
	move.l	a0,v_adjcharbuffer(a5)		; precalculated value

	lea	ring1data(pc),a0		;
	move.b	#5,v_rings+0*sizeofring+ring_id(a5)
	move.l	a0,v_rings+0*sizeofring+ring_datap(a5)
	move.w	#740*chunk,v_rings+0*sizeofring+ring_range(a5)
	move.w	#37-1,v_rings+0*sizeofring+ring_numchars(a5)
	move.w	#20*chunk,v_rings+0*sizeofring+ring_delta(a5)
	move.w	#2*chunk,v_rings+0*sizeofring+ring_advance(a5)

	lea	ring2data(pc),a0		;
	move.b	#4,v_rings+1*sizeofring+ring_id(a5)
	move.l	a0,v_rings+1*sizeofring+ring_datap(a5)
	move.w	#360*chunk,v_rings+1*sizeofring+ring_range(a5)
	move.w	#30-1,v_rings+1*sizeofring+ring_numchars(a5)
	move.w	#12*chunk,v_rings+1*sizeofring+ring_delta(a5)
	move.w	#chunk,v_rings+1*sizeofring+ring_advance(a5)

	lea	ring3data(pc),a0		;
	move.b	#3,v_rings+2*sizeofring+ring_id(a5)
	move.l	a0,v_rings+2*sizeofring+ring_datap(a5)
	move.w	#350*chunk,v_rings+2*sizeofring+ring_range(a5)
	move.w	#25-1,v_rings+2*sizeofring+ring_numchars(a5)
	move.w	#14*chunk,v_rings+2*sizeofring+ring_delta(a5)
	move.w	#chunk,v_rings+2*sizeofring+ring_advance(a5)

	lea	ring4data(pc),a0		;
	move.b	#2,v_rings+3*sizeofring+ring_id(a5)
	move.l	a0,v_rings+3*sizeofring+ring_datap(a5)
	move.w	#340*chunk,v_rings+3*sizeofring+ring_range(a5)
	move.w	#17-1,v_rings+3*sizeofring+ring_numchars(a5)
	move.w	#20*chunk,v_rings+3*sizeofring+ring_delta(a5)
	move.w	#chunk,v_rings+3*sizeofring+ring_advance(a5)

	lea	ring5data(pc),a0		;
	move.b	#1,v_rings+4*sizeofring+ring_id(a5)
	move.l	a0,v_rings+4*sizeofring+ring_datap(a5)
	move.w	#330*chunk,v_rings+4*sizeofring+ring_range(a5)
	move.w	#15-1,v_rings+4*sizeofring+ring_numchars(a5)
	move.w	#22*chunk,v_rings+4*sizeofring+ring_delta(a5)
	move.w	#chunk,v_rings+4*sizeofring+ring_advance(a5)

	lea	ring6data(pc),a0		;
;	move.b	#0,v_rings+5*sizeofring+ring_id(a5)
	move.l	a0,v_rings+5*sizeofring+ring_datap(a5)
	move.w	#320*chunk,v_rings+5*sizeofring+ring_range(a5)
	move.w	#10-1,v_rings+5*sizeofring+ring_numchars(a5)
	move.w	#32*chunk,v_rings+5*sizeofring+ring_delta(a5)
	move.w	#chunk,v_rings+5*sizeofring+ring_advance(a5)

	move.w	#10,v_scrollpos(a5)		; prevent possible visible trash
	move.w	#60*2,v_mbcolsoffset(a5)	;

	move.w	#6,v_taxideltaa(a5)		;
	move.w	#4,v_taxideltab(a5)		;
	move.w	#2,v_taxideltac(a5)		;
	move.w	#8,v_taxideltad(a5)		;
	rts					;


*------	GENERATE SINE TABLE ---------------------------------------------------*

; sine table, 1024 angle steps, factor 256

generatesintab
	lea	.sinb(pc),a0			;
	move.l	b_sintab(pc),a1			;
	move.w	#246-1,d7			;
.gensin	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	move.w	d0,(a1)+			;
	dbf	d7,.gensin			;

	move.l	a1,a0				; used for cos
	moveq	#10+11-1,d7			; 10 values for sin, 11 values for cos
.fill256
	move.w	#$0100,(a1)+			;
	dbf	d7,.fill256			;

	move.w	#245-1,d7			; cos
.gencos	move.w	-(a0),(a1)+			;
	dbf	d7,.gencos			;

	move.w	#512-1,d7			;
	move.l	b_sintab(pc),a0			;
.genneg	move.w	(a0)+,d0			;
	neg.w	d0				;
	move.w	d0,(a1)+			;
	dbf	d7,.genneg			;

	move.l	b_sintab(pc),a0			;
	move.w	#256-1,d7			;
.gensin2
	move.w	(a0)+,(a1)+			;
	dbf	d7,.gensin2			;
	rts					;

.sinb	dc.b	$00,$02,$03,$05,$06,$08,$09,$0b
	dc.b	$0d,$0e,$10,$11,$13,$14,$16,$18
	dc.b	$19,$1b,$1c,$1e,$1f,$21,$22,$24
	dc.b	$26,$27,$29,$2a,$2c,$2d,$2f,$30
	dc.b	$32,$33,$35,$37,$38,$3a,$3b,$3d
	dc.b	$3e,$40,$41,$43,$44,$46,$47,$49
	dc.b	$4a,$4c,$4d,$4f,$50,$52,$53,$55
	dc.b	$56,$58,$59,$5b,$5c,$5e,$5f,$61
	dc.b	$62,$63,$65,$66,$68,$69,$6b,$6c
	dc.b	$6d,$6f,$70,$72,$73,$75,$76,$77
	dc.b	$79,$7a,$7b,$7d,$7e,$80,$81,$82
	dc.b	$84,$85,$86,$88,$89,$8a,$8c,$8d
	dc.b	$8e,$90,$91,$92,$93,$95,$96,$97
	dc.b	$99,$9a,$9b,$9c,$9e,$9f,$a0,$a1
	dc.b	$a2,$a4,$a5,$a6,$a7,$a8,$aa,$ab
	dc.b	$ac,$ad,$ae,$af,$b1,$b2,$b3,$b4
	dc.b	$b5,$b6,$b7,$b8,$b9,$ba,$bc,$bd
	dc.b	$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5
	dc.b	$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd
	dc.b	$ce,$cf,$cf,$d0,$d1,$d2,$d3,$d4
	dc.b	$d5,$d6,$d7,$d7,$d8,$d9,$da,$db
	dc.b	$dc,$dc,$dd,$de,$df,$e0,$e0,$e1
	dc.b	$e2,$e3,$e3,$e4,$e5,$e5,$e6,$e7
	dc.b	$e7,$e8,$e9,$e9,$ea,$eb,$eb,$ec
	dc.b	$ed,$ed,$ee,$ee,$ef,$ef,$f0,$f1
	dc.b	$f1,$f2,$f2,$f3,$f3,$f4,$f4,$f5
	dc.b	$f5,$f5,$f6,$f6,$f7,$f7,$f8,$f8
	dc.b	$f8,$f9,$f9,$f9,$fa,$fa,$fa,$fb
	dc.b	$fb,$fb,$fc,$fc,$fc,$fd,$fd,$fd
	dc.b	$fd,$fd,$fe,$fe,$fe,$fe,$fe,$ff
	dc.b	$ff,$ff,$ff,$ff,$ff,$ff		; 246 values 

	even

sintab	dc.w	$0008,$0008,$0008,$0008,$0008,$0008,$0008,$0008
	dc.w	$0008,$0008,$0008,$0009,$0009,$0009,$0009,$0009
	dc.w	$0009,$0009,$0009,$0009,$0009,$0009,$0009,$0009
	dc.w	$0009,$0009,$0009,$0009,$0009,$0009,$0009,$000a
	dc.w	$000a,$000a,$000a,$000a,$000a,$000a,$000a,$000a
	dc.w	$000a,$000a,$000a,$000a,$000a,$000a,$000a,$000a
	dc.w	$000a,$000a,$000a,$000a,$000b,$000b,$000b,$000b
	dc.w	$000b,$000b,$000b,$000b,$000b,$000b,$000b,$000b
	dc.w	$000b,$000b,$000b,$000b,$000b,$000b,$000b,$000b
	dc.w	$000b,$000b,$000c,$000c,$000c,$000c,$000c,$000c
	dc.w	$000c,$000c,$000c,$000c,$000c,$000c,$000c,$000c
	dc.w	$000c,$000c,$000c,$000c,$000c,$000c,$000c,$000c
	dc.w	$000c,$000c,$000d,$000d,$000d,$000d,$000d,$000d
	dc.w	$000d,$000d,$000d,$000d,$000d,$000d,$000d,$000d
	dc.w	$000d,$000d,$000d,$000d,$000d,$000d,$000d,$000d
	dc.w	$000d,$000d,$000d,$000d,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$0010,$0010,$0010,$0010,$0010,$0010
	dc.w	$0010,$0010,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$000f,$000f,$000f,$000f,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000e,$000e,$000e
	dc.w	$000e,$000e,$000e,$000e,$000e,$000d,$000d,$000d
	dc.w	$000d,$000d,$000d,$000d,$000d,$000d,$000d,$000d
	dc.w	$000d,$000d,$000d,$000d,$000d,$000d,$000d,$000d
	dc.w	$000d,$000d,$000d,$000d,$000d,$000d,$000d,$000c
	dc.w	$000c,$000c,$000c,$000c,$000c,$000c,$000c,$000c
	dc.w	$000c,$000c,$000c,$000c,$000c,$000c,$000c,$000c
	dc.w	$000c,$000c,$000c,$000c,$000c,$000c,$000c,$000b
	dc.w	$000b,$000b,$000b,$000b,$000b,$000b,$000b,$000b
	dc.w	$000b,$000b,$000b,$000b,$000b,$000b,$000b,$000b
	dc.w	$000b,$000b,$000b,$000b,$000b,$000a,$000a,$000a
	dc.w	$000a,$000a,$000a,$000a,$000a,$000a,$000a,$000a
	dc.w	$000a,$000a,$000a,$000a,$000a,$000a,$000a,$000a
	dc.w	$000a,$000a,$0009,$0009,$0009,$0009,$0009,$0009
	dc.w	$0009,$0009,$0009,$0009,$0009,$0009,$0009,$0009
	dc.w	$0009,$0009,$0009,$0009,$0009,$0009,$0008,$0008
	dc.w	$0008,$0008,$0008,$0008,$0008,$0008,$0008,$0008
	dc.w	$0008,$0008,$0008,$0008,$0008,$0008,$0008,$0008
	dc.w	$0008,$0008,$0008,$0007,$0007,$0007,$0007,$0007
	dc.w	$0007,$0007,$0007,$0007,$0007,$0007,$0007,$0007
	dc.w	$0007,$0007,$0007,$0007,$0007,$0007,$0007,$0006
	dc.w	$0006,$0006,$0006,$0006,$0006,$0006,$0006,$0006
	dc.w	$0006,$0006,$0006,$0006,$0006,$0006,$0006,$0006
	dc.w	$0006,$0006,$0006,$0006,$0005,$0005,$0005,$0005
	dc.w	$0005,$0005,$0005,$0005,$0005,$0005,$0005,$0005
	dc.w	$0005,$0005,$0005,$0005,$0005,$0005,$0005,$0005
	dc.w	$0005,$0005,$0004,$0004,$0004,$0004,$0004,$0004
	dc.w	$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0004
	dc.w	$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0004
	dc.w	$0004,$0004,$0003,$0003,$0003,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0003,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0003,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0003,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0003,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0003,$0003,$0003,$0004
	dc.w	$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0004
	dc.w	$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0004
	dc.w	$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0005
	dc.w	$0005,$0005,$0005,$0005,$0005,$0005,$0005,$0005
	dc.w	$0005,$0005,$0005,$0005,$0005,$0005,$0005,$0005
	dc.w	$0005,$0005,$0005,$0005,$0005,$0006,$0006,$0006
	dc.w	$0006,$0006,$0006,$0006,$0006,$0006,$0006,$0006
	dc.w	$0006,$0006,$0006,$0006,$0006,$0006,$0006,$0006
	dc.w	$0006,$0006,$0007,$0007,$0007,$0007,$0007,$0007
	dc.w	$0007,$0007,$0007,$0007,$0007,$0007,$0007,$0007
	dc.w	$0007,$0007,$0007,$0007,$0007,$0007,$0008,$0008
	dc.w	$0008,$0008,$0008,$0008,$0008,$0008,$0008,$0008
	

*------ PRINT TEXT ------------------------------------------------------------*

; param a0 text, a1 bitplane, d4 xpos, d5.l width
printtext
.char	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	beq	.done				;

	sub.b	#" ",d0				;
	moveq	#0,d1				;
	move.b	prop(pc,d0.w),d1		;
	addq.w	#2,d1				; extra char spacing

	asl.w	#5,d0				; *32 (data of 1 char)
	lea	font(pc),a2			;
	add.w	d0,a2				;

	moveq	#15,d3				; bit 15
.col	movem.l	a1-a2,-(a7)			;
	moveq	#visiblefontheight-1,d7		;
.row	move.w	(a2),d0				;
	btst.l	d3,d0				;
	beq	.no				;
	or.b	d4,(a1)				; draw pixel
.no	addq.w	#2,a2				; next row of char
	add.l	d5,a1				;
	dbf	d7,.row				;

	movem.l	(a7)+,a1-a2			;
	tst.w	d3				;
	beq	.eod				;
	subq.w	#1,d3				; next bit to test in char col
.eod	ror.b	#1,d4				; x++
	bcc	.x1				;
	addq.w	#1,a1				; next byte
.x1	dbf	d1,.col				;

	tst.b	v_doquit(a5)			;
	bne	.done				;

	bra	.char				;

.done	rts					;

; net values, 0 = unused char
prop	dc.b	12	; space
	dc.b	3	; !
	dc.b	8	; "
	dc.b	0	; #
	dc.b	0	; $
	dc.b	0	; %
	dc.b	0	; &
	dc.b	3	; '
	dc.b	7	; (
	dc.b	7	; )
	dc.b	12	; *
	dc.b	0	; +
	dc.b	0	; ,
	dc.b	12	; -
	dc.b	3	; .
	dc.b	9	; /
	dc.b	0,4,12,0,12,12,0,0,0,12	; 0-9
	dc.b	3	; :
	dc.b	0	; ;
	dc.b	0	; <
	dc.b	0	; =
	dc.b	0	; >
	dc.b	13	; ?
	dc.b	13	; @ (used as 13px wide space)
	dc.b	12,12,12,12	; ABCD
	dc.b	12,12,12,12	; EFGH
	dc.b	3,12,12,12	; IJKL
	dc.b	13,12,12,12	; MNOP
	dc.b	12,12,12,11	; QRST
	dc.b	12,13,13,12	; UVWX
	dc.b	13,12		; YZ


*------ TEXT ------------------------------------------------------------------*

text	dc.b	"                       "
	dc.b	"WE'LL SHOW YOU ANOTHER ROTOZOOMER.                     "
	dc.b	"WE'RE JUST JOKING.                     "
	dc.b	"SPREADPOINT PRESENTS                     "
	dc.b	"SPREADPOINTS                     "
	dc.b	"RELEASED AT MOUNTAINBYTES 2O25                       "
	dc.b	"MUSIC: LORD                       "
	dc.b	"LOGO: ORTHO*                       "
	dc.b	"CODE AND FONT: DEPECHE                     "
	dc.b	0

greetingslist
	dc.b	"                       "
	dc.b	"ZYMOSIS   "
	dc.b	"ZODIAC   "
	dc.b	"VOID   "
	dc.b	"VF   "
	dc.b	"UP ROUGH   "
	dc.b	"TRSI   "
	dc.b	"TRBL   "
	dc.b	"TPOLM   "
	dc.b	"TTE   "
	dc.b	"TEK   "
	dc.b	"TBL   "
	dc.b	"SPECTROX   "
	dc.b	"SPACEBALLS   "
	dc.b	"SOFTWARE FAILURE   "
	dc.b	"SLIPSTREAM   "
	dc.b	"SCOOPEX   "
	dc.b	"SCA   "
	dc.b	"RESISTANCE   "
	dc.b	"REBELS   "
	dc.b	"POUET   "
	dc.b	"OXYGENE   "
	dc.b	"NGC   "
	dc.b	"NAH-KOLOR   "
	dc.b	"MELON   "
	dc.b	"MCCOY   "
	dc.b	"LOONIES   "
	dc.b	"LEMON.   "
	dc.b	"LEMMY   "
	dc.b	"KESTRA BITWORLD   "
	dc.b	"ISTARI   "
	dc.b	"GHOSTOWN   "
	dc.b	"FFP   "
	dc.b	"ECHTZEIT   "
	dc.b	"DESIRE   "
	dc.b	"DEMOZOO   "
	dc.b	"DEKADENCE   "
	dc.b	"DEADLINERS   "	
	dc.b	"COSMIC ORBS   "
	dc.b	"BINARY   "
	dc.b	"BATMAN GROUP   "
	dc.b	"ATTENTIONWHORE   "
	dc.b	"ARTSTATE   "
	dc.b	"ANDROMEDA   "
	dc.b	"AMIGABILL   "
	dc.b	"ALTAIR   "
	dc.b	"ALCATRAZ   "
	dc.b	"AFWD   "
	dc.b	"ABYSS-CONNECTION   "
	dc.b	"ABYSS                    "
	dc.b	0


*------ FONT ------------------------------------------------------------------*

fontheight	equ 16
	even

font	ds.w	16	; space

	dc.w	%1110000000000000	; !
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110011100000000	; "
	dc.w	%1110011100000000
	dc.w	%1110011100000000
	dc.w	%1110011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; #
	ds.w	16	; $
	ds.w	16	; %
	ds.w	16	; &

	dc.w	%1110000000000000	; '
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111110000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111110000000000
	dc.w	%1111110000000000
	dc.w	%0111110000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111110000000000	; )
	dc.w	%0111111000000000	; extra padding at beginning
	dc.w	%0111111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0111111000000000
	dc.w	%0111111000000000
	dc.w	%0111110000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0110011001100000	; *
	dc.w	%0011011011000000
	dc.w	%0001111110000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0001111110000000
	dc.w	%0011011011000000
	dc.w	%0110011001100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; +
	ds.w	16	; ,

	dc.w	%0000000000000000	; -
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000	; .
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000011100000000	; /
	dc.w	%0000011100000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%0011100000000000
	dc.w	%0011100000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; 0 (use O instead)

	dc.w	%0111000000000000	; 1
	dc.w	%0111000000000000	; extra padding at beginning
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0111000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000	; 2
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; 3

	dc.w	%1110000001110000	; 4
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111110000	; 5
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; 6
	ds.w	16	; 7
	ds.w	16	; 8

	dc.w	%0111111111100000	; 9
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000	; :
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	ds.w	16	; ;
	ds.w	16	; <
	ds.w	16	; =
	ds.w	16	; >

	dc.w	%1111111111110000	; ?
	dc.w	%1111111111111000
	dc.w	%1111111111111000
	dc.w	%0000000000111000
	dc.w	%0000000000111000
	dc.w	%0000011111111000
	dc.w	%0000011111111000
	dc.w	%0000011111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	
	dc.w	%0000000000000000	; @ (used as 13px wide space)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111100000	; A
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110011111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000011100000
	dc.w	%1110000111000000
	dc.w	%1110001110000000
	dc.w	%1110011100000000
	dc.w	%1111111000000000
	dc.w	%1111110000000000
	dc.w	%1111111000000000
	dc.w	%1110011100000000
	dc.w	%1110001110000000
	dc.w	%1110000111000000
	dc.w	%1110000011100000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1111000001111000
	dc.w	%1111100011111000
	dc.w	%1111110111111000
	dc.w	%1111111111111000
	dc.w	%1110111110111000
	dc.w	%1110011100111000
	dc.w	%1110001000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	
	dc.w	%1110000001110000
	dc.w	%1111000001110000
	dc.w	%1111100001110000
	dc.w	%1111110001110000
	dc.w	%1111111001110000
	dc.w	%1110111101110000
	dc.w	%1110011111110000
	dc.w	%1110001111110000
	dc.w	%1110000111110000
	dc.w	%1110000011110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	
	dc.w	%0111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%1110011100000000
	dc.w	%1110001110000000
	dc.w	%1110000111000000
	dc.w	%1110000011100000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111100000
	dc.w	%1111111111110000
	dc.w	%0111111111110000
	dc.w	%0000000001110000
	dc.w	%0000000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111100000
	dc.w	%1111111111100000
	dc.w	%1111111111100000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000111000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0111111111100000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1111000001111000
	dc.w	%0111100011110000
	dc.w	%0011110111100000
	dc.w	%0001111111000000
	dc.w	%0000111110000000
	dc.w	%0000011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110001000111000
	dc.w	%1110011100111000
	dc.w	%1110111110111000
	dc.w	%1111111111111000
	dc.w	%1111110111111000
	dc.w	%1111100011111000
	dc.w	%1111000001111000
	dc.w	%1110000000111000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0111111111100000
	dc.w	%0111111111100000
	dc.w	%0111111111100000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%1110000001110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1110000000111000
	dc.w	%1111111111111000
	dc.w	%1111111111111000
	dc.w	%0111111111110000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000111100000
	dc.w	%0000001111000000
	dc.w	%0000011110000000
	dc.w	%0000111100000000
	dc.w	%0001111000000000
	dc.w	%0011110000000000
	dc.w	%0111100000000000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%1111111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

; one char = 16*16 hypotenuse = 22.6 -> 24
		
	ds.b	6
rotfont	ds.b	4*3				; S
	dc.b	%00000001,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000001,%11111111,%11000000
	dc.b	%00000000,%00000001,%11000000
	dc.b	%00000000,%00000001,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; P
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; R
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%10011100,%00000000
	dc.b	%00000011,%10001110,%00000000
	dc.b	%00000011,%10000111,%00000000
	dc.b	%00000011,%10000011,%10000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; E
	dc.b	%00000001,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%10000000,%00000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000001,%11111111,%11000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3
	dc.b	%00000001,%11111111,%10000000	; A
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; D
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; O
	dc.b	%00000001,%11111111,%10000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000011,%11111111,%11000000
	dc.b	%00000001,%11111111,%10000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; I
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; N
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%11000001,%11000000
	dc.b	%00000011,%11100001,%11000000
	dc.b	%00000011,%11110001,%11000000
	dc.b	%00000011,%11111001,%11000000
	dc.b	%00000011,%10111101,%11000000
	dc.b	%00000011,%10011111,%11000000
	dc.b	%00000011,%10001111,%11000000
	dc.b	%00000011,%10000111,%11000000
	dc.b	%00000011,%10000011,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000011,%10000001,%11000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3

	ds.b	4*3				; T
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000011,%11111111,%10000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00111000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	dc.b	%00000000,%00000000,%00000000
	ds.b	4*3


*------ INIT TAXI EFFECT -------------------------------------------------*

inittaxieffect
	moveq	#0,d7				; loop 0...255
	lea	v_taxicollut(a5),a1		;
.lut	moveq	#0,d0				;
	move.w	#255,d0				;
	sub.w	d7,d0				; adjust
	divu.w	#26,d0				;
	move.b	d0,(a1)+			;
	addq.b	#1,d7				;
	bne	.lut				;

	lea	v_taxidata(a5),a4		;
	move.l	b_charbuffer(pc),a0		; rotated chars
	add.w	#cuttop*cbwidth,a0		;

	moveq	#1,d1				; y (2 = centering)
	moveq	#taxirows-1,d7			; rows
.looprows
	lea	charoffsets(pc),a1		;
	moveq	#18,d0				; x (18 = centering)
	moveq	#taxicolumns-1,d6		; columns
.loopcolumns
	move.w	d0,(a4)+			; x
	move.w	d1,(a4)+			; y

	move.l	a0,a2				; char
	add.l	(a1)+,a2			;
	move.l	a2,(a4)+			;

	add.w	#taxideltax,d0			; advance x
	dbf	d6,.loopcolumns			;
	
	add.w	#taxideltay,d1			; advance y
	dbf	d7,.looprows			;
	rts					;

charoffsets
	rept 7 ; 7*10 chars > num chars on largest rings
	dc.l	0*rotationsteps*charheight*cbwidth	; S
	dc.l	1*rotationsteps*charheight*cbwidth	; P
	dc.l	2*rotationsteps*charheight*cbwidth	; R
	dc.l	3*rotationsteps*charheight*cbwidth	; E
	dc.l	4*rotationsteps*charheight*cbwidth	; A
	dc.l	5*rotationsteps*charheight*cbwidth	; D
	dc.l	1*rotationsteps*charheight*cbwidth	; P
	dc.l	6*rotationsteps*charheight*cbwidth	; O
	dc.l	7*rotationsteps*charheight*cbwidth	; I
	dc.l	8*rotationsteps*charheight*cbwidth	; N
	dc.l	9*rotationsteps*charheight*cbwidth	; T
	endr


*------ ROTATE FONT ------------------------------------------------------*

rotatefont
	lea	12,a4				; constant
	lea	24,a2				; constant

	move.l	b_charbuffer(pc),a3		;
	moveq	#0,d7				; angle "360-d7"

.loopc	moveq	#1,d3				; start x (pixel)
	ror.b	d3,d3				; d3 = %1000 0000

	lea	sinetable+90(pc),a0		;
	move.w	-90(a0,d7.w),d2			; sin a
	move.w	90(a0,d7.w),d4			; cos a

	moveq	#-12,d1				; y coordinate
.loopy	moveq	#-12,d0				; x coordinate
.loopx	movem.l	d0/d1,-(a7)			;
	move.l	d0,d5				; copy of x
	move.l	d1,d6				; copy of y
	
	muls	d4,d5				; d5 = x cos a
	muls	d2,d6				; d6 = y sin a
	add.l	d5,d6				; d6 = x' = x cos a + y sin a
	asr.l	#8,d6				; div 256
	
	muls	d2,d0				; d0 = x sin a
	muls	d4,d1				; d1 = y cos a
	sub.l	d0,d1				; d1 = y' = -x sin a + y cos a (y cos a - x sin a)
	asr.l	#8,d1				; div 256

	add.l	a4,d6				; translate x'
	bmi	.notset				; out of bounds?
	cmp.l	a2,d6				;
	bge	.notset				;
		
	add.l	a4,d1				; translate y'
	bmi	.notset				;
	cmp.l	a2,d1				;
	bge	.notset				;

	move.l	d1,d0				; d1 *= 3
	add.l	d1,d1				;
	add.l	d0,d1				;

	move.l	d6,d0				;
	asr.l	#3,d0				; make byte offset
	add.w	d0,d1				; d1 = offset to byte
	not.b	d6				; bit in byte to test

	lea	rotfont-6(pc),a0		; -6 = adjust center
	move.l	a3,a1				; copy to scratch
	moveq	#numchars-1,d0			;
.loopchars
	btst	d6,(a0,d1.w)			;
	beq	.void				;
	or.b	d3,(a1)				; draw pixel
.void	add.w	#charsourcesize,a0		; next source char
	add.w	#rotationsteps*charheight*cbwidth,a1 ; next char rotated destination
	dbf	d0,.loopchars			;

.notset	ror.b	#1,d3				; x++
	bcc	.byte				;
	addq.w	#1,a3				; next byte
.byte	movem.l	(a7)+,d0/d1			;
	addq.l	#1,d0				; next x
	cmp.l	a4,d0				; done with column?
	bne	.loopx				;

	moveq	#1,d3				; reset start x (pixel)
	ror.b	d3,d3				; d3 = %1000 0000
	add.l	#cbwidth-3,a3			; next row in destination bitmap

	addq.l	#1,d1				; next y
	cmp.l	a4,d1				; done with rows?
	bne	.loopy				;

	tst.b	v_doquit(a5)			;
	bne	.done				;

	add.w	#5*2,d7				; next angle (5 degrees step)
	cmp.w	#360*2,d7			;
	bne	.loopc				;
.done	rts					;


*------	SINE TABLE -------------------------------------------------------*

; AMICOM'S SINE TABLE	1988-03-03 BY AMICOM AND DEPECHE

sinetable
	dc.w $0000,$0004,$0009,$000d,$0012,$0016,$001b,$001f
	dc.w $0024,$0028,$002c,$0031,$0035,$003a,$003e,$0042
	dc.w $0047,$004b,$004f,$0053,$0058,$005c,$0060,$0064
	dc.w $0068,$006c,$0070,$0074,$0078,$007c,$0080,$0084
	dc.w $0088,$008b,$008f,$0093,$0096,$009a,$009e,$00a1
	dc.w $00a5,$00a8,$00ab,$00af,$00b2,$00b5,$00b8,$00bb
	dc.w $00be,$00c1,$00c4,$00c7,$00ca,$00cc,$00cf,$00d2
	dc.w $00d4,$00d7,$00d9,$00db,$00de,$00e0,$00e2,$00e4
	dc.w $00e6,$00e8,$00ea,$00ec,$00ed,$00ef,$00f1,$00f2
	dc.w $00f3,$00f5,$00f6,$00f7,$00f8,$00f9,$00fa,$00fb
	dc.w $00fc,$00fd,$00fe,$00fe,$00ff,$00ff,$00ff,$0100
	dc.w $0100,$0100,$0100,$0100,$0100,$0100,$00ff,$00ff
	dc.w $00ff,$00fe,$00fe,$00fd,$00fc,$00fb,$00fa,$00f9
	dc.w $00f8,$00f7,$00f6,$00f5,$00f3,$00f2,$00f1,$00ef
	dc.w $00ed,$00ec,$00ea,$00e8,$00e6,$00e4,$00e2,$00e0
	dc.w $00de,$00db,$00d9,$00d7,$00d4,$00d2,$00cf,$00cc
	dc.w $00ca,$00c7,$00c4,$00c1,$00be,$00bb,$00b8,$00b5
	dc.w $00b2,$00af,$00ab,$00a8,$00a5,$00a1,$009e,$009a
	dc.w $0096,$0093,$008f,$008b,$0088,$0084,$0080,$007c
	dc.w $0078,$0074,$0070,$006c,$0068,$0064,$0060,$005c
	dc.w $0058,$0053,$004f,$004b,$0047,$0042,$003e,$003a
	dc.w $0035,$0031,$002c,$0028,$0024,$001f,$001b,$0016
	dc.w $0012,$000d,$0009,$0004,$0000,$fffc,$fff7,$fff3
	dc.w $ffee,$ffea,$ffe5,$ffe1,$ffdc,$ffd8,$ffd4,$ffcf
	dc.w $ffcb,$ffc6,$ffc2,$ffbe,$ffb9,$ffb5,$ffb1,$ffad
	dc.w $ffa8,$ffa4,$ffa0,$ff9c,$ff98,$ff94,$ff90,$ff8c
	dc.w $ff88,$ff84,$ff80,$ff7c,$ff78,$ff75,$ff71,$ff6d
	dc.w $ff6a,$ff66,$ff62,$ff5f,$ff5b,$ff58,$ff55,$ff51
	dc.w $ff4e,$ff4b,$ff48,$ff45,$ff42,$ff3f,$ff3c,$ff39
	dc.w $ff36,$ff34,$ff31,$ff2e,$ff2c,$ff29,$ff27,$ff25
	dc.w $ff22,$ff20,$ff1e,$ff1c,$ff1a,$ff18,$ff16,$ff14
	dc.w $ff13,$ff11,$ff0f,$ff0e,$ff0d,$ff0b,$ff0a,$ff09
	dc.w $ff08,$ff07,$ff06,$ff05,$ff04,$ff03,$ff02,$ff02
	dc.w $ff01,$ff01,$ff01,$ff00,$ff00,$ff00,$ff00,$ff00
	dc.w $ff00,$ff00,$ff01,$ff01,$ff01,$ff02,$ff02,$ff03
	dc.w $ff04,$ff05,$ff06,$ff07,$ff08,$ff09,$ff0a,$ff0b
	dc.w $ff0d,$ff0e,$ff0f,$ff11,$ff13,$ff14,$ff16,$ff18
	dc.w $ff1a,$ff1c,$ff1e,$ff20,$ff22,$ff25,$ff27,$ff29
	dc.w $ff2c,$ff2e,$ff31,$ff34,$ff36,$ff39,$ff3c,$ff3f
	dc.w $ff42,$ff45,$ff48,$ff4b,$ff4e,$ff51,$ff55,$ff58
	dc.w $ff5b,$ff5f,$ff62,$ff66,$ff6a,$ff6d,$ff71,$ff75
	dc.w $ff78,$ff7c,$ff80,$ff84,$ff88,$ff8c,$ff90,$ff94
	dc.w $ff98,$ff9c,$ffa0,$ffa4,$ffa8,$ffad,$ffb1,$ffb5
	dc.w $ffb9,$ffbe,$ffc2,$ffc6,$ffcb,$ffcf,$ffd4,$ffd8
	dc.w $ffdc,$ffe1,$ffe5,$ffea,$ffee,$fff3,$fff7,$fffc

	dc.w $0000,$0004,$0009,$000d,$0012,$0016,$001b,$001f
	dc.w $0024,$0028,$002c,$0031,$0035,$003a,$003e,$0042
	dc.w $0047,$004b,$004f,$0053,$0058,$005c,$0060,$0064
	dc.w $0068,$006c,$0070,$0074,$0078,$007c,$0080,$0084
	dc.w $0088,$008b,$008f,$0093,$0096,$009a,$009e,$00a1
	dc.w $00a5,$00a8,$00ab,$00af,$00b2,$00b5,$00b8,$00bb
	dc.w $00be,$00c1,$00c4,$00c7,$00ca,$00cc,$00cf,$00d2
	dc.w $00d4,$00d7,$00d9,$00db,$00de,$00e0,$00e2,$00e4
	dc.w $00e6,$00e8,$00ea,$00ec,$00ed,$00ef,$00f1,$00f2
	dc.w $00f3,$00f5,$00f6,$00f7,$00f8,$00f9,$00fa,$00fb
	dc.w $00fc,$00fd,$00fe,$00fe,$00ff,$00ff,$00ff,$0100
	dc.w $0100,$0100,$0100,$0100,$0100,$0100,$00ff,$00ff
	dc.w $00ff,$00fe,$00fe,$00fd,$00fc,$00fb,$00fa,$00f9
	dc.w $00f8,$00f7,$00f6,$00f5,$00f3,$00f2,$00f1,$00ef
	dc.w $00ed,$00ec,$00ea,$00e8,$00e6,$00e4,$00e2,$00e0
	dc.w $00de,$00db,$00d9,$00d7,$00d4,$00d2,$00cf,$00cc
	dc.w $00ca,$00c7,$00c4,$00c1,$00be,$00bb,$00b8,$00b5
	dc.w $00b2,$00af,$00ab,$00a8,$00a5,$00a1,$009e,$009a
	dc.w $0096,$0093,$008f,$008b,$0088,$0084,$0080,$007c
	dc.w $0078,$0074,$0070,$006c,$0068,$0064,$0060,$005c
	dc.w $0058,$0053,$004f,$004b,$0047,$0042,$003e,$003a
	dc.w $0035,$0031,$002c,$0028,$0024,$001f,$001b,$0016
	dc.w $0012,$000d,$0009,$0004,$0000,$fffc,$fff7,$fff3
	dc.w $ffee,$ffea,$ffe5,$ffe1,$ffdc,$ffd8,$ffd4,$ffcf
	dc.w $ffcb,$ffc6,$ffc2,$ffbe,$ffb9,$ffb5,$ffb1,$ffad
	dc.w $ffa8,$ffa4,$ffa0,$ff9c,$ff98,$ff94,$ff90,$ff8c
	dc.w $ff88,$ff84,$ff80,$ff7c,$ff78,$ff75,$ff71,$ff6d
	dc.w $ff6a,$ff66,$ff62,$ff5f,$ff5b,$ff58,$ff55,$ff51
	dc.w $ff4e,$ff4b,$ff48,$ff45,$ff42,$ff3f,$ff3c,$ff39
	dc.w $ff36,$ff34,$ff31,$ff2e,$ff2c,$ff29,$ff27,$ff25
	dc.w $ff22,$ff20,$ff1e,$ff1c,$ff1a,$ff18,$ff16,$ff14
	dc.w $ff13,$ff11,$ff0f,$ff0e,$ff0d,$ff0b,$ff0a,$ff09
	dc.w $ff08,$ff07,$ff06,$ff05,$ff04,$ff03,$ff02,$ff02
	dc.w $ff01,$ff01,$ff01,$ff00,$ff00,$ff00,$ff00,$ff00
	dc.w $ff00,$ff00,$ff01,$ff01,$ff01,$ff02,$ff02,$ff03
	dc.w $ff04,$ff05,$ff06,$ff07,$ff08,$ff09,$ff0a,$ff0b
	dc.w $ff0d,$ff0e,$ff0f,$ff11,$ff13,$ff14,$ff16,$ff18
	dc.w $ff1a,$ff1c,$ff1e,$ff20,$ff22,$ff25,$ff27,$ff29
	dc.w $ff2c,$ff2e,$ff31,$ff34,$ff36,$ff39,$ff3c,$ff3f
	dc.w $ff42,$ff45,$ff48,$ff4b,$ff4e,$ff51,$ff55,$ff58
	dc.w $ff5b,$ff5f,$ff62,$ff66,$ff6a,$ff6d,$ff71,$ff75
	dc.w $ff78,$ff7c,$ff80,$ff84,$ff88,$ff8c,$ff90,$ff94
	dc.w $ff98,$ff9c,$ffa0,$ffa4,$ffa8,$ffad,$ffb1,$ffb5
	dc.w $ffb9,$ffbe,$ffc2,$ffc6,$ffcb,$ffcf,$ffd4,$ffd8
	dc.w $ffdc,$ffe1,$ffe5,$ffea,$ffee,$fff3,$fff7,$fffc


*------	PRINT OUT OF MEMORY ---------------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


;*****************************************************************
;
;	Light Speed Player v1.13 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	X: @leonard_coder
;
;	--------How to use--------- 
;
;	bsr LSP_MusicDriver_CIA_Start : Init LSP player code and install CIA interrupt
;		a2: VBR (CPU Vector Base Register) ( use 0 if 68000 )
;
;	bsr LSP_MusicDriver_CIA_Stop : Stop LSP music replay
;*****************************************************************

LSP_MusicDriver_CIA_Start
	lea	.irqVector(pc),a3
	lea	$78(a2),a2
	move.l	a2,(a3)
	lea	.LSPDmaCon+1(pc),a2		; DMACON byte patch address
	bsr	LSP_MusicInit			; init the LSP player ( whatever fast or insane version )

	lea	.pMusicBPM(pc),a2
	move.l	a0,(a2)				; store music BPM pointer
	move.w	(a0),d0				; start BPM
	lea	.curBpm(pc),a2
	move.w	d0,(a2)

; d0: music BPM
;.LSP_IrqInstall
	move.w	#(1<<13),$dff09a		; disable CIA interrupt
	lea	.LSP_MainIrq(pc),a0
	move.l	.irqVector(pc),a4
	move.l	a0,(a4)

	lea	$bfd000,a0
	move.b 	#$7f,$d00(a0)
	move.b 	#$10,$e00(a0)
	move.b 	#$10,$f00(a0)
	move.l	#1773447,d1			; PAL clock

	lea	.ciaClock(pc),a4
	move.l	d1,(a4)
	divu.w	d0,d1
	move.b	d1,$400(a0)
	lsr.w 	#8,d1
	move.b	d1,$500(a0)
	move.b	#$83,$d00(a0)
	move.b	#$11,$e00(a0)
			
	move.b	#496&255,$600(a0)		; set timer B to 496 (to set DMACON)
	move.b	#496>>8,$700(a0)

	move.w 	#(1<<13),$dff09c		; clear any req CIA
	move.w 	#$a000,$dff09a			; CIA interrupt enabled
	rts

.LSPDmaCon	dc.w	$8000
.irqVector	dc.l	0
.ciaClock	dc.l	0
.curBpm		dc.w	0
.pMusicBPM	dc.l	0

.LSP_MainIrq
	btst.b	#0,$bfdd00
	beq	.skipa

	movem.l	d0-a6,-(a7)
	lea	custom,a6
	bsr	LSP_MusicPlayTick		; LSP main music driver tick

	; check if BMP changed in the middle of the music
.mute	move.l	.pMusicBPM(pc),a0
	move.w	(a0),d0				; current music BPM
	cmp.w	.curBpm(pc),d0
	beq	.noChg
	lea	.curBpm(pc),a2			
	move.w	d0,(a2)				; current BPM
	move.l	.ciaClock(pc),d1
	divu.w	d0,d1
	move.b	d1,$bfd400
	lsr.w 	#8,d1
	move.b	d1,$bfd500			

.noChg	lea	.LSP_DmaconIrq(pc),a0
	move.l	.irqVector(pc),a1
	move.l	a0,(a1)
	move.b	#$19,$bfdf00			; start timer B, one shot

	movem.l	(a7)+,d0-a6
.skipa	move.w	#$2000,$dff09c
	nop
	rte

.LSP_DmaconIrq
	btst.b	#1,$bfdd00
	beq	.skipb
	move.w	.LSPDmaCon(pc),$dff096
	pea	(a0)
	move.l	.irqVector(pc),a0
	pea	.LSP_MainIrq(pc)
	move.l	(a7)+,(a0)
	move.l	(a7)+,a0
.skipb	move.w	#$2000,$dff09c
	nop
	rte

LSP_MusicDriver_CIA_Stop
	move.b	#$7f,$bfdd00
	move.w	#$2000,$9a(a6)
	move.w	#$2000,$9c(a6)
	move.w	#$000f,$96(a6)
	rts


;------------------------------------------------------------------
;
;	LSP_MusicInit
;
;		In:	a0: LSP music data (any memory)
;			a1: LSP sound bank (chip memory)
;			a2: DMAcon patch
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;------------------------------------------------------------------
LSP_MusicInit
	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),d1		; a1: sound bank data (chip mem)

	lea	LSP_State(pc),a3
	move.l	a2,m_dmaconPatch(a3)
	
	move.l	a0,a4				; relocation flag ad
	addq.w	#2,a0				; skip relocation flag
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.w	(a0)+,m_escCodeGetPos(a3)
	move.l	(a0)+,-(a7)			; music len in frame ticks
	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (win 2 cycles in insane player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a0,a1				; keep relocated flag
.relocLoop
	tst.b	(a4)				; relocation guard
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes table size
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0

	; read sequence timing infos (if any)
	move.w	(a0)+,m_seqCount(a3)
	beq	.noSeq
	move.l	a0,m_seqTable(a3)
	clr.w	m_currentSeq(a3)
	move.w	m_seqCount(a3),d0
	moveq	#0,d1
	move.w	d0,d1
	lsl.w	#3,d1				; 8 bytes per entry
	add.w	#12,d1				; add 3 last 32bits (word stream size, byte stream loop, word stream loop)
	add.l	a0,d1				; word stream data address
	subq.w	#1,d0
.seqRel	tst.b	(a4)
	bne	.skipRel
	add.l	d1,(a0)
	add.l	d1,4(a0)
.skipRel
	addq.w	#8,a0
	dbf	d0,.seqRel

.noSeq	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	st	(a4)				; mark this music score as "relocated"
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks
	rts


;------------------------------------------------------------------
;
;	LSP_MusicPlayTick
;
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;------------------------------------------------------------------
LSP_MusicPlayTick
	lea	LSP_State(pc),a1
	move.l	m_byteStream(a1),a0
	move.l	m_codeTableAddr(a1),a2
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
.cmdExec
	add.b	d0,d0
	bcc	.noVd
	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	.resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	add.w	(a0)+,a2
	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3	; set dmacon value
	move.b	d1,(a3)

.noInst	move.l	a0,(a1)			; store word stream (or byte stream if coming from early out)
	rts

.cextended
	add.w	#$100,d0
	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0		; code
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm
	cmp.w	m_escCodeGetPos(a1),d0
	bne	.cmdExec
.r_setPos
	move.b	(a0)+,(m_currentSeq+1)(a1)
	bra	.process

.r_rewind	
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm
	move.b	(a0)+,(m_currentBpm+1)(a1)	; BPM
	bra	.process

.resetv	dc.l	0,0,0,0

	rsreset	
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 dmacon
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_seqCount		rs.w	1
m_seqTable		rs.l	1
m_currentSeq		rs.w	1
m_escCodeGetPos		rs.w	1
sizeof_LSPVars		rs.w	0

LSP_State	ds.b	sizeof_LSPVars
	even


*------	LOGO COLOR FADING -----------------------------------------------------*

logofading
	CHECKACTOR actor_logo_fading
	beq	.done			;

	move.w	v_logocolindex(a5),d0	;
	btst	#0,d0			; every second frame only
	bne	.skip			;
	move.l	b_clist5(pc),a1		;
	add.w	#clist5cols-clist5+2,a1	;

	lea	.numcolors(pc),a2	;
	lea	.logocols(pc,d0.w),a0	;
.loop	move.w	(a2)+,d7		;
	bmi	.endoflist		; end of list (-1)?
.set	move.w	(a0),(a1)		; same color to many color registers
	addq.w	#4,a1			; next color in clist
	dbf	d7,.set			;
	add.w	#.logocol1end-.logocol1,a0 ; next color bank
	bra	.loop			;

.endoflist
.skip	tst.w	v_logofadestate(a5)	; fade in or out?
	bne	.fadeout		;

	addq.w	#1,v_logocolindex(a5)	;
	cmp.w	#.logocol1end-.logocol1,v_logocolindex(a5) ;
	bne	.nf			;
	STOPACTOR actor_logo_fading
	subq.w	#2,v_logocolindex(a5)	; adjust index
	st	v_logofadestate(a5)	; prepare fading out
	rts				;

.fadeout
	subq.w	#1,v_logocolindex(a5)	;
	cmp.w	#-2,v_logocolindex(a5)	;
	bne	.nf			;
	STOPACTOR actor_logo_fading
.nf	
.done	rts				;

.numcolors
	dc.w	1-1	; $0188
	dc.w	1-1	; $018a
	dc.w	1-1	; $018e
	dc.w	8-1	; $0190 - $019e
	dc.w	-1	; end of list

.logocols
.logocol1
	; $0188
	dc.w    $0000,$0111,$0122,$0223,$0234,$0335,$0346,$0457,$0458,$0569,$056a,$067b,$068c,$068d,$079d,$079e
.logocol1end
	; $018a
	dc.w    $0000,$0111,$0111,$0112,$0113,$0113,$0224,$0224,$0225,$0225,$0226,$0336,$0337,$0337,$0338,$0338
	; $018e
	dc.w    $0000,$0111,$0122,$0133,$0234,$0245,$0256,$0366,$0367,$0378,$0489,$048a,$049b,$05ac,$05bd,$05bd
	; $0190,$0192,$0194,$0196,$0198,$019a,$019c,$019e
	dc.w    $0000,$0111,$0222,$0333,$0334,$0444,$0555,$0666,$0667,$0778,$0888,$0889,$099a,$0aab,$0bbc,$0bbc


*------	RINGS DATA ------------------------------------------------------------*

	include	"ringsdata.s"
	even


*------	LOGO ------------------------------------------------------------------*

logo	incbin	"logo"
logoend
	even


*------	MUSIC -----------------------------------------------------------------*

lspbank	incbin	"lord-lorder-shorter.lsbank"
lspbankend
	even	; very important

lspmusic
	incbin	"lord-lorder-shorter.lsmusic",10 ; skip header (10 bytes)
	even
