;-----------------------------------------------------------------------------	
; 
; Header for the kernel recovery file.
; It's aheader that acts as an identifier and tells the size of the kernel.
; This file has to be saved in the SD card after formatting it,
; then it's loaded from the recovery menu.
;
; Manuel Pazos 13/01/2013
;
;-----------------------------------------------------------------------------	

    org 0
		;----------------
	db	"MFRSD KERNEL 1.0"
	db	128/8   ;Size of the kernel ROM file in 8K blocks
	ds	512-$,#ff

    ;The full Nextor kernel ROM with the MFRSD driver goes here
