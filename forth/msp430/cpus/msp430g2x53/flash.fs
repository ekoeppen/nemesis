$0128 constant FCTL1
$012A constant FCTL2
$012C constant FCTL3

: unlock-flash      $A500 FCTL3 ! ;
: lock-flash        $A510 FCTL3 ! ;

: erase-segment     ( segment -- )
                    dint unlock-flash $A502 FCTL1 !
                    0 swap !
                    $A500 FCTL1 ! lock-flash eint ;

: erase-to-end      ( segment -- )
                    erase-segment ;

: set-flash-clk     ( prescaler -- )
                    $A580 + FCTL2 ! lock-flash ;

: +flash-write      unlock-flash   $A540 FCTL1 ! ;

: (clear-info)      init-cold dup pad #12 cells move   erase-segment ;
: (restore-info)    unlock-flash   +flash-write pad init-cold #12 cells move ;

: save              (clear-info)
                    latest @ pad 2+ !   dp @ pad 4 + !   vp @ pad 6 + !
                    (restore-info) ;

: restore           (clear-info)
                    6 cells pad + pad 6 cells move
                    (restore-info)
                    init-dp @ erase-to-end
                    reset-handler ;
