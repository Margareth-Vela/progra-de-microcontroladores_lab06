    ;Archivo:	    Lab6.s
    ;Dispositivo:   PIC16F887
    ;Autor:	    Margareth Vela
    ;Compilador:    pic-as(v2.31), MPLABX V5.45
    ;
    ;Programa:	    Temporizadores
    ;Hardware:	    Displays 7 seg en puerto C, transistores en puerto D 
    ;		    & led en puerto E
    ;Creado: 23 mar 2021
    ;Última modificación: 23 mar, 2021
    


PROCESSOR 16F887
#include <xc.inc>

; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscilador interno sin salidas
  CONFIG  WDTE = OFF            ; WDT disabled (reinicio dispositivo del pic)
  CONFIG  PWRTE = ON            ; PWRT enabled (espera de 72ms al iniciar)
  CONFIG  MCLRE = OFF           ; El pin de MCLR se utiliza como I/O
  CONFIG  CP = OFF              ; Sin protección de código
  CONFIG  CPD = OFF             ; Sin protección de datos
  CONFIG  BOREN = OFF           ; Sin reinicio cuándo el voltaje de alimentacion baja de 4v
  CONFIG  IESO = OFF            ; Reinicio sin cambio de reloj de interno a externo
  CONFIG  FCMEN = OFF           ; Cambio de reloj externo a interno en caso de fallo
  CONFIG  LVP = ON              ; Programacion en bajo voltaje permitida

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Reinicio abajo de 4V, (BOR21V=2.1V)
  CONFIG  WRT = OFF             ; Protección de autoescritura por el programa desactivada
    
;-------------------------------------------------------------------------------
; Macros
;-------------------------------------------------------------------------------    
  
reiniciar_tmr0 macro ; macro para reutilizar reinicio de tmr0
    Banksel PORTA
    movlw 252 ; valor de n para (256-n)
    movwf TMR0 ; delay inicial TMR0
    bcf T0IF
endm
    
reiniciar_tmr1 macro	; Reinicio de Timer1
    Banksel PORTA   
    movlw   0x85   ; Cargar valor de registro W
    movwf   TMR1H  ; Mover el valor de W al byte más significativo del TMR1
    movlw   0xEE   ; Cargar valor de registro W, valor inicial del tmr0
    movwf   TMR1L  ; Mover el valor de W al byte menos significativo del TMR1
    bcf	    TMR1IF    ; Limpiar bit de interrupción por overflow (Bit de INTCON)	
    endm

;-------------------------------------------------------------------------------
; Variables 
;-------------------------------------------------------------------------------
PSECT udata_bank0 ;common memory
    var: DS 1 ;1 byte 
    banderas: DS 1 ;1 byte 
    nibble: DS 2; variables para contador hexademimal
    displays: DS 2	
   
    
PSECT udata_shr ;common memory
    W_TEMP: DS 1 ;1 byte
    STATUS_TEMP: DS 1; 1 byte
    
;-------------------------------------------------------------------------------
; Vector reset
;-------------------------------------------------------------------------------
PSECT resVect, class=CODE, abs, delta=2
ORG 00h
resetVec:
    PAGESEL main
    goto main
    
PSECT intVect, class=CODE, abs, delta=2
;-------------------------------------------------------------------------------
; Vector de interrupción
;-------------------------------------------------------------------------------
ORG 04h 

push: ;Preservar los valores de W y las banderas
    movwf W_TEMP
    swapf STATUS, W
    movwf STATUS_TEMP 
    
isr: ; rutina de interrupción
    btfsc   T0IF	; Si está encendida la bandera, entonces
    call    int_tmr0	; va a la subrutina del TMR0
    btfsc   TMR1IF	; Si está encendida la bandera, entonces
    call    int_tmr1	; va a la subrutina del TMR1 
    btfsc   TMR2IF	; Si está encendida la bandera, entonces
    call    int_tmr2	; va a la subrutina del TMR2 

pop: ; para re-obtener los valores de W y de las banderas de status
    swapf STATUS_TEMP, W
    movwf STATUS
    swapf W_TEMP, F
    swapf W_TEMP, W
    retfie ; finalizar interrupción
    

;-------------------------------------------------------------------------------
; Sub rutinas para interrupciones
;-------------------------------------------------------------------------------
int_tmr1:
    reiniciar_tmr1
    incf    PORTA   ;Incrementa el puerto
    return

int_tmr2:
    incf    PORTE   ;Incrementa el puerto
    bcf	    TMR2IF  ;Apaga la bandera de interrupción
    return
    
int_tmr0: 
    reiniciar_tmr0
    clrf PORTD 
    btfss PORTE, 0 ;Si el bit 0 de PORTE está encendido, entonces enciende los
    return	   ; displays, de lo contrario regresa a la rutina principal
    btfsc banderas,0
    goto display_1
    
    
display_0: ; display del nibble menos significativo
    movf displays+0,W
    movwf PORTC
    bsf PORTD,0 ;habilitar pin 0 de PORTD para encender display 0
    goto next_display 

display_1: ; display del nibble más significativo
    movf displays+1,W
    movwf PORTC
    bsf PORTD,1 ;habilitar pin 1 de PORTD para encender display 1
    goto next_display
    
next_display: ; subrutina para ir iterando entre cada uno de los displays
    movlw 1
    xorwf banderas,F 
    return
    
;-------------------------------------------------------------------------------
; Código Principal
;-------------------------------------------------------------------------------
PSECT code, delta=2, abs
ORG 100h ;Posición para el código
 
tabla: 
    clrf PCLATH
    bsf PCLATH, 0   ; PCLATH = 01 PCL = 02
    andlw 0x0f	    ; Para solo llegar hasta f
    addwf PCL	    ; PC = PCLATH + PCL + offset
    retlw 00111111B ;0
    retlw 00000110B ;1
    retlw 01011011B ;2
    retlw 01001111B ;3
    retlw 01100110B ;4
    retlw 01101101B ;5
    retlw 01111101B ;6
    retlw 00000111B ;7
    retlw 01111111B ;8
    retlw 01101111B ;9
    retlw 01110111B ;A
    retlw 01111100B ;B
    retlw 00111001B ;C
    retlw 01011110B ;D
    retlw 01111001B ;E
    retlw 01110001B ;F 

;-------------------------------------------------------------------------------
; Configuraciones
;-------------------------------------------------------------------------------   
main:
    
    call config_io	; Configuración de I/O
    call config_reloj   ; Configuración del reloj
    call config_tmr0	; Configuración del TMR0
    call config_tmr1	; Configuración del TMR1
    call config_tmr2    ; Configuración del TMR2
    call config_int_enable ; Configuración de Interrupciones
        
loop:
   movf PORTA, W   ; Pasar valor de contador a display hexadecimal
   movwf var
   call separar_nibbles 
   call preparar_displays  
   goto loop	   ;loop forever

;-------------------------------------------------------------------------------
; Sub rutinas
;-------------------------------------------------------------------------------
 separar_nibbles:
    movf    var, 0	; Mueve el valor de la variable al registro W
    andlw   0x0f	; Solamente toma los primeros 4 bits de la variable
    movwf   nibble	; Mueve el valor de la variable a nibble
    swapf   var, 0	; Cambia los bytes de la variable var
    andlw   0x0f	; Solamente toma los primeros 4 bits de la variable
    movwf   nibble+1	; Mueve el valor de la variable al segundo byte de nibble
    return

preparar_displays:
    movf    nibble, 0	
    call    tabla
    movwf   displays	; Se guarda el valor del primer byte de nibble en el primer byte display
    movf    nibble+1, 0
    call    tabla
    movwf   displays+1	; Se guarda el valor del segundo byte de nibble en el segundo byte display
    return 

;-------------------------------------------------------------------------------
; Subrutinas de configuración
;-------------------------------------------------------------------------------
config_io:
    banksel ANSEL ;Banco 11
    clrf    ANSEL ;Pines digitales
    clrf    ANSELH
    
    banksel TRISA ;Banco 01
    clrf    TRISA ;Salida del Contador
    clrf    TRISC ;Display multiplexados 
    clrf    TRISD ;Alternancia de displays
    bcf    TRISE, 0 ;Modo intermitente en led
    
    banksel PORTA ;Banco 00
    clrf    PORTA ;Comenzar contador binario en 0
    clrf    PORTC ;Comenzar displays en 0
    clrf    PORTD 
    bcf    PORTE, 0
    return
    
config_int_enable:; INTCON
    Banksel PORTA
    bsf	GIE	; Se habilitan las interrupciones globales
    bsf	PEIE	
    bsf	T0IE    ; Se habilitan la interrupción del TMR0
    bcf	T0IF    ; Se limpia la bandera
    Banksel TRISA
    bsf	TMR1IE	; Se habilitan la interrupción del TMR1 y TMR2 Registro PIE1
    bsf	TMR2IE   
    Banksel PORTA
    bcf	TMR1IF  ; Se limpian las banderas de interrupción Registro PIR1
    bcf	TMR2IF
    return  
   
config_reloj:	; Configuración de reloj interno
    Banksel OSCCON  
    bsf	    IRCF2   
    bcf	    IRCF1
    bcf	    IRCF0   ; Configuración del oscilador a 1MHz
    bsf	    SCS	    ; Seleccionar el reloj interno
    return

config_tmr0:
    banksel TRISA
    bcf T0CS ; reloj interno
    bcf PSA ; prescaler
    bsf PS2 
    bsf PS1 
    bsf PS0 ; PS = 111 (1:256)
    banksel PORTA
    reiniciar_tmr0
    return
    
config_tmr1:
    Banksel PORTA  
    bsf	    TMR1ON  ; Se habilita el TMR1
    bcf	    TMR1CS  ; Seleccion del reloj interno
    bsf	    T1CKPS0
    bsf	    T1CKPS1 ; Prescaler a 1:8
    reiniciar_tmr1
    return
   
config_tmr2:
    banksel PORTA
    bsf	    TMR2ON  ; Se habilita el TMR2
    bsf	    T2CKPS1
    bsf	    T2CKPS0 ; Prescaler a 1:16
    bsf	    TOUTPS3
    bsf	    TOUTPS2
    bsf	    TOUTPS1
    bsf	    TOUTPS0 ;Postscaler 1:16
    bcf	    TMR2IF  ; Limpiar bit de interrupción por overflow (Bit de INTCON)
    Banksel TRISA   
    movlw   244   ; Cargar valor de registro W, valor de PR2
    movwf   PR2	
    return      
end