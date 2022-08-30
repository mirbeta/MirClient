(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)

  {$if defined(nucleof401re) }
    {$define stm32f401re}
  {$endif}

  {$if defined(nucleof411re) }
    {$define stm32f411re}
  {$endif}

  {$if defined(nucleof446re) }
    {$define stm32f446re}
  {$endif}

  {$if defined(discoveryf401vc) }
    {$define stm32f401vc}
  {$endif}

  {$if defined(discoveryf407vg) }
    {$define stm32f407vg}
  {$endif}

  {$if defined(discoveryf411ve) }
    {$define stm32f411re}
  {$endif}

  {$if defined(discoveryf429zi) }
    {$define stm32f429zi}
  {$endif}

{$if defined(discoveryf469ni) }
  {$define stm32f469ni}
{$endif}

  {$if defined(stm32f401cb) or defined(stm32f401cc) or defined(stm32f401cd) or defined(stm32f401ce) }
    {$define stm32f401xx}
    {$define lqfp48}
  {$endif}

  {$if defined(stm32f401rb) or defined(stm32f401rc) or defined(stm32f401rd) or defined(stm32f401re) }
    {$define stm32f401xx}
    {$define lqfp64}
  {$endif}

  {$if defined(stm32f401vb) or defined(stm32f401vc) or defined(stm32f401vd) or defined(stm32f401ve) }
    {$define stm32f401xx}
    {$define lqfp100}
  {$endif}

  {$if defined(stm32f405ie) or defined(stm32f405ig)}
    {$define stm32f405xx}
    {$define lqfp176}
  {$endif}

  {$if defined(stm32f405ve) or defined(stm32f405vg)}
    {$define stm32f405xx}
    {$define lqfp100}
  {$endif}

  {$if defined(stm32f405ze) or defined(stm32f405zg)}
    {$define stm32f405xx}
    {$define lqfp144}
  {$endif}

  {$if defined(stm32f407ie) or defined(stm32f407ig)}
    {$define stm32f407xx}
    {$define lqfp176}
  {$endif}

  {$if defined(stm32f407ve) or defined(stm32f407vg)}
    {$define stm32f407xx}
    {$define lqfp100}
  {$endif}

  {$if defined(stm32f407ze) or defined(stm32f407zg)}
    {$define stm32f407xx}
    {$define lqfp144}
  {$endif}

  {$if defined(stm32f411cc) or defined(stm32f411ce)}
    {$define stm32f411xx}
    {$define lqfp48}
  {$endif}


{$if defined(stm32f411rc) or defined(stm32f411re)}
  {$define stm32f411xx}
  {$define lqfp64}
{$endif}

{$if defined(stm32f411vc) or defined(stm32f411ve)}
  {$define stm32f411xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f415ie) or defined(stm32f415ig)}
  {$define stm32f415xx}
  {$define lqfp176}
{$endif}

{$if defined(stm32f415ve) or defined(stm32f415vg)}
  {$define stm32f415xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f415ze) or defined(stm32f415zg)}
  {$define stm32f415xx}
  {$define lqfp144}
{$endif}

{$if defined(stm32f417ie) or defined(stm32f417ig)}
  {$define stm32f417xx}
  {$define lqfp176}
{$endif}

{$if defined(stm32f417ve) or defined(stm32f417vg)}
  {$define stm32f417xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f417ze) or defined(stm32f417zg)}
  {$define stm32f417xx}
  {$define lqfp144}
{$endif}

{$if defined(stm32f427ie) or defined(stm32f427ig)}
  {$define stm32f427xx}
  {$define lqfp176}
{$endif}

{$if defined(stm32f427ve) or defined(stm32f427vg)}
  {$define stm32f427xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f427ze) or defined(stm32f427zg)}
  {$define stm32f427xx}
  {$define lqfp144}
{$endif}

{$if defined(stm32f429ie) or defined(stm32f429ig)}
  {$define stm32f429xx}
  {$define lqfp176}
{$endif}

{$if defined(stm32f427ve) or defined(stm32f427vg)}
  {$define stm32f427xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f427ze) or defined(stm32f427zg)}
  {$define stm32f427xx}
  {$define lqfp144}
{$endif}

{$if defined(stm32f429ie) or defined(stm32f429ig)}
  {$define stm32f429xx}
  {$define lqfp176}
{$endif}

{$if defined(stm32f429ve) or defined(stm32f429vg)}
  {$define stm32f429xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f429ze) or defined(stm32f429zg)}
  {$define stm32f429xx}
  {$define lqfp144}
{$endif}

{$if defined(stm32f446re) or defined(stm32f446rc)}
  {$define stm32f446xx}
  {$define lqfp64}
{$endif}

{$if defined(stm32f446ve) or defined(stm32f446vc)}
  {$define stm32f446xx}
  {$define lqfp100}
{$endif}

{$if defined(stm32f446ze) or defined(stm32f446zc)}
  {$define stm32f446xx}
  {$define lqfp144}
{$endif}

{$if defined(stm32f469ae) or defined(stm32f469ag) or defined(stm32f469ai)}
  {$define stm32f469xx}
  {$define ufbga169}
{$endif}

{$if defined(stm32f469be) or defined(stm32f469bg) or defined(stm32f469bi)}
  {$define stm32f469xx}
  {$define lqfp208}
{$endif}

{$if defined(stm32f469ie) or defined(stm32f469ig) or defined(stm32f469ii)}
  {$define stm32f469xx}
  {$define bga176}
{$endif}

{$if defined(stm32f469ne) or defined(stm32f469ng) or defined(stm32f469ni)}
  {$define stm32f469xx}
  {$define tfbga176}
{$endif}

{$if defined(stm32f479ag) or defined(stm32f479ai)}
  {$define stm32f479xx}
  {$define ufbga169}
{$endif}

{$if defined(stm32f479bg) or defined(stm32f479bi)}
  {$define stm32f479xx}
  {$define lqfp208}
{$endif}

{$if defined(stm32f479ig) or defined(stm32f479ii)}
  {$define stm32f479xx}
  {$define bga176}
{$endif}

{$if defined(stm32f479ng) or defined(stm32f4679ni)}
  {$define stm32f479xx}
  {$define tfbga176}
{$endif}


{$define has_gpioa}

{$define has_gpiob}
{$define has_gpiob11}

{$define has_gpioc}
{$define has_gpioc0_c12}

{$if defined(lqfp48)}
  {$undef has_gpioc0_c12}
{$endif}

{$define has_gpiod}
{$define has_gpiod01d03_d15}

{$if defined(lqfp48)}
  {$undef has_gpiod}
  {$undef has_gpiod01d03_d15}
{$endif}

{$if defined(lqfp64)}
  {$undef has_gpiod01d03_d15}
{$endif}

{$if defined(lqfp100) or defined(lqfp144) or defined(lqfp176) or defined(lqfp208)
  or defined(ufbga169) or defined(bga176) or defined(tfbga176)}
  {$define has_gpiod}
{$endif}

{$if defined(lqfp100) or defined(lqfp144) or defined(lqfp176) or defined(lqfp208)
  or defined(ufbga169) or defined(bga176) or defined(tfbga176)}
  {$define has_gpioe}
{$endif}

{$if defined(lqfp144) or defined(lqfp176) or defined(lqfp208)
  or defined(ufbga169) or defined(bga176) or defined(tfbga176)}
  {$define has_gpiof}
{$endif}

{$if defined(lqfp144) or defined(lqfp176) or defined(lqfp208)
  or defined(ufbga169) or defined(bga176) or defined(tfbga176)}
  {$define has_gpiog}
{$endif}

{$define has_gpioh}

{$if defined(lqfp176) or defined(lqfp208)
  or defined(ufbga169) or defined(bga176) or defined(tfbga176)}
  {$define has_gpioh02_h15}
{$endif}

{$if defined(lqfp176) or defined(lqfp208)
  or defined(ufbga169) or defined(bga176) or defined(tfbga176)}
  {$define has_gpioi}
{$endif}

type
  TNativePin = record
  const
    PA0 =0;   PA1 =1;   PA2 =2;   PA3 =3;   PA4 =4;   PA5 =5;   PA6 =6;   PA7 =7;
    PA8 =8;   PA9 =9;   PA10=10;  PA11=11;  PA12=12;  PA13=13;  PA14=14;  PA15=15;
    PB0 =16;  PB1 =17;  PB2 =18;  PB3 =19;  PB4 =20;  PB5 =21;  PB6 =22;  PB7 =23;
    PB8 =24;  PB9 =25;  PB10=26;  PB11=27;  PB12=28;  PB13=29;  PB14=30;  PB15=31;
    PC0 =32;  PC1 =33;  PC2 =34;  PC3 =35;  PC4 =36;  PC5 =37;  PC6 =38;  PC7 =39;
    PC8 =40;  PC9 =41;  PC10=42;  PC11=43;  PC12=44;  PC13=45;  PC14=46;  PC15=47;
    {$if defined (has_gpiod)}
    PD0 =48;  PD1 =49;  PD2 =50;  PD3 =51;  PD4 =52;  PD5 =53;  PD6 =54;  PD7 =55;
    PD8 =56;  PD9 =57;  PD10=58;  PD11=59;  PD12=60;  PD13=61;  PD14=62;  PD15=63;
    {$endif}
    {$if defined (has_gpioe)}
    PE0 =64;  PE1 =65;  PE2 =66;  PE3 =67;  PE4 =68;  PE5 =69;  PE6 =70;  PE7 =71;
    PE8 =72;  PE9 =73;  PE10=74;  PE11=75;  PE12=76;  PE13=77;  PE14=78;  PE15=79;
    {$endif}
    {$if defined (has_gpiof)}
    PF0 =80;  PF1 =81;  PF2 =82;  PF3 =83;  PF4 =84;  PF5 =85;  PF6 =86;  PF7 =87;
    PF8 =88;  PF9 =89;  PF10=90;  PF11=91;  PF12=92;  PF13=93;  PF14=94;  PF15=95;
    {$endif}
    {$if defined (has_gpiog)}
    PG0 =96;  PG1 =97;  PG2 =98;  PG3 =99;  PG4 =100; PG5 =101; PG6 =102; PG7 =103;
    PG8 =104; PG9 =105; PG10=106; PG11=107; PG12=108; PG13=109; PG14=110; PG15=111;
    {$endif}
    PH0 =112; PH1 =113; PH2 =114; PH3 =115; PH4 =116; PH5 =117; PH6 =118; PH7 =119;
    PH8 =120; PH9 =121; PH10=122; PH11=123; PH12=124; PH13=125; PH14=126; PH15=127;
    {$if defined (has_gpioi)}
    PI0 =128; PI1 =129; PI2 =130; PI3 =131; PI4 =132; PI5 =133; PI6 =134; PI7 =135;
    PI8 =136; PI9 =137; PI10=138; PI11=139; PI12=140; PI13=141; PI14=142; PI15=143;
    {$endif}
    NONE=$ffffffff;
  end;


{$define has_adc1}
{$define has_adc2}
{$define has_adc3}

 {$if defined(stm32f405xx) or defined(stm32f415xx) or defined(stm32f407xx) or defined(stm32f417xx)
  or defined(stm32f427xx) or defined(stm32f437xx) or defined(stm32f429xx) or defined(stm32f439xx) or defined(stm32f446xx)
  or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_can1}
  {$define has_can2}
{$endif}

{$if defined(stm32f405xx) or defined(stm32f415xx) or defined(stm32f407xx) or defined(stm32f417xx)
  or defined(stm32f427xx) or defined(stm32f437xx) or defined(stm32f429xx) or defined(stm32f439xx) or defined(stm32f446xx)
  or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_dac1}
  {$define has_dac2}
{$endif}

{$define has_i2c1}
{$define has_i2c2}
{$define has_i2c3}

{$define has_spi1}
{$define has_spi2}
{$define has_spi3}
{$if defined(stm32f401xx) or defined(stm32f411xx) or defined(stm32f427xx) or defined(stm32f437xx)
      or defined(stm32f429xx) or defined(stm32f439xx) or defined(stm32f446xx) or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_spi4}
{$endif}

{$if defined(lqfp64)}
  {$undef has_spi4}
{$endif}


{$if defined(stm32f411xx) or defined(stm32f427xx) or defined(stm32f437xx)
  or defined(stm32f429xx) or defined(stm32f439xx) or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_spi5}
{$endif}
{$if defined(stm32f427xx) or defined(stm32f437xx)  or defined(stm32f429xx) or defined(stm32f439xx)
  or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_spi6}
{$endif}
type
  TSPI1_MOSIPins= record
  const
    PA7=TNativePin.PA7; PB5=TNativePin.PB5;
  end;

  TSPI1_MISOPins= record
  const
    PA6=TNativePin.PA6; PB4=TNativePin.PB4;
  end;

  TSPI1_SCLKPins= record
  const
    PA5=TNativePin.PA5; PB3=TNativePin.PB3;
  end;

  TSPI1_NSSPins= record
  const
    None=TNativePin.None; PA4=TNativePin.PA4; PA15=TNativePin.PA15;
  end;


  TSPI2_MOSIPins= record
  const
    PB15=TNativePin.PB15; PC3=TNativePin.PC3; {$if defined (has_gpioi)}PI3=TNativePin.PI3;{$endif}
  end;

  TSPI2_MISOPins= record
  const
    PB14=TNativePin.PB14; PC2=TNativePin.PC2; {$if defined (has_gpioi)}PI2=TNativePin.PI2;{$endif}
  end;

  TSPI2_SCLKPins= record
  const
    PB10=TNativePin.PB10; PB13=TNativePin.PB13; {$if defined (has_gpioi)}PI1=TNativePin.PI1;{$endif}
  end;
  //PD3 f401
  TSPI2_NSSPins= record
  const
    None=TNativePin.None; PB9=TNativePin.PB9; PB12=TNativePin.PB12; {$if defined (has_gpioi)}PI0=TNativePin.PI0;{$endif}
  end;


{$if defined(has_spi3)}
  TSPI3_MOSIPins= record
  const
    PB5=TNativePin.PB5; PC12=TNativePin.PC12;
  end;
 //PD6 f401
  TSPI3_MISOPins= record
  const
    PB4=TNativePin.PB4; PC11=TNativePin.PC11;
  end;

  TSPI3_SCLKPins= record
  const
    PB3=TNativePin.PB3; PC10=TNativePin.PC10;
  end;

  TSPI3_NSSPins = record
  const
    None=TNativePin.None; PA4=TNativePin.PA4; PA15=TNativePin.PA15;
  end;
{$endif}

{$if defined(has_spi4)}
  TSPI4_MOSIPins= record
  const
    PE6=TNativePin.PE6; PE14=TNativePin.PE14;
  end;

  TSPI4_MISOPins= record
  const
    PE5=TNativePin.PE5; PE13=TNativePin.PE13;
  end;

  TSPI4_SCLKPins= record
  const
    PE2=TNativePin.PE2; PE12=TNativePin.PE12;
  end;

  TSPI4_NSSPins = record
  const
    None=TNativePin.None; PE4=TNativePin.PE4; PE11=TNativePin.PE11;
  end;

{$endif}

{$if defined(has_spi5)}
{$endif}

{$if defined(has_spi6)}
{$endif}


{$define has_usart1}
{$define has_usart2}
{$if defined(stm32f405xx) or defined(stm32f415xx) or defined(stm32f407xx) or defined(stm32f417xx)
  or defined(stm32f429xx) or defined(stm32f439xx) or defined(stm32f446xx) or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_usart3}
  {$define has_usart4}
  {$define has_usart5}
{$endif}

{$define has_usart6}

{$if defined(stm32f427xx) or defined(stm32f437xx)  or defined(stm32f429xx) or defined(stm32f439xx)
  or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define has_usart7}
  {$define has_usart8}
{$endif}

{$if defined(stm32f405xx) or defined(stm32f407xx) or defined(stm32f415xx) or defined(stm32f417xx)}
  {$define stm32f4xx}
{$endif}

{$if defined (stm32f401xx) or defined(stm32f411xx) or defined (stm32f4xx) or defined(stm32f446xx)
  or defined(stm32f469xx) or defined(stm32f479xx)}
  {$define stm32f4}
{$else}
  {$error Definition for controller missing, make sure that the controller is listed in PXL.STM32F4Config.inc
  and that you have configured it correctly on commandline e.g.: fpc -dnucleof401re -Wpnucleof401re }
{$endif}


TUSART1_RXPins= record
const
  PA10 = TNativePin.PA10; PB7 = TNativePin.PB7;
end;

TUSART1_TXPins= record
const
  PA9 = TNativePin.PA10;
  PB6 = TNativePin.PB6;
end;

TUSART2_RXPins= record
const
  PA3 = TNativePin.PA3; {$if defined(has_gpiod01d03_d15)}PD6 = TNativePin.PD6;{$endif}
end;

TUSART2_TXPins= record
const
  PA2 = TNativePin.PA2; {$if defined(has_gpiod01d03_d15)}PD5 = TNativePin.PD5;{$endif}
end;

{$if defined(has_usart3)}
TUSART3_RXPins=(PB11,PC11{$if defined(has_gpiod)},PD9{$endif});
TUSART3_TXPins=(PB10,PC10{$if defined(has_gpiod)},PD8{$endif});
{$endif}

{$if defined(has_usart4)}
TUSART4_RXPins=(PA1,PC11);
TUSART4_TXPins=(PA0,PC10);
{$endif}

{$if defined(has_usart5) and defined(has_gpiod)}
TUSART5_RXPins=(PD2);
TUSART5_TXPins=(PC12);
{$endif}

TUSART6_RXPins= record
const
  {$if defined(stm32f401xx)}PA12 = TNativePin.PA12;{$endif} PC7 = TNativePin.PC7;
  {$if defined(has_gpiog)} PG9 = TNativePin.PG9;{$endif}
end;

TUSART6_TXPins= record
const
  {$if defined(stm32f401xx)}PA11 = TNativePin.PA11;{$endif} PC6 = TNativePin.PC6;
  {$if defined(has_gpiog)} PG14 = TNativePin.PG14;{$endif}
end;

{$if defined(has_usart7)}
TUSART7_RXPins= record
const
  PE7 = TNativePin.PE7; PF6 = TNativePin.PF6;
end;
TUSART7_TXPins= record
const
  PE8 = TNativePin.PE8; PF7 = TNativePin.PF7;
end;
{$endif}

{$if defined(has_usart8)}
TUSART8_RXPins= record
const
  PE0 = TNativePin.PE0;
end;
TUSART8_TXPins= record
const
  PE1 = TNativePin.PE1;
end;
{$endif}


{$if defined(nucleof401re) or defined(nucleof411re) or defined(nucleof446re)}
type
  TArduinoPin = record
  const
    D0 =TNativePin.PA3;  D1 =TNativePin.PA2;  D2 =TNativePin.PA10;  D3 =TNativePin.PB3;
    D4 =TNativePin.PB5;  D5 =TNativePin.PB4;  D6 =TNativePin.PB10;  D7 =TNativePin.PA8;
    D8 =TNativePin.PA9;  D9 =TNativePin.PC7;  D10=TNativePin.PB6;   D11=TNativePin.PA7;
    D12=TNativePin.PA6;  D13=TNativePin.PA5;  D14=TNativePin.PB9;   D15=TNativePin.PB8;

    A0 =TNativePin.PA0;  A1 =TNativePin.PA1;  A2 =TNativePin.PA4;   A3 =TNativePin.PB0;
    A4 =TNativePin.PC1;  A5 =TNativePin.PC0;
    NONE=$ffffffff;
  end;

  TMorphoCN7Pin = record
  const
    P1 =TNativePin.PC10; P2 =TNativePin.PC11; P3 =TNativePin.PC12; P4 =TNativePin.PD2;
    P13=TNativePin.PA13; P15=TNativePin.PA14; P17=TNativePin.PA15; P21=TNativePin.PB7;
    P23=TNativePin.PC13; P25=TNativePin.PC14; P27=TNativePin.PC15; P28=TNativePin.PA0;
    P29=TNativePin.PH0;  P30=TNativePin.PA1;  P31=TNativePin.PH1;  P32=TNativePin.PA4;
    P34=TNativePin.PB0;  P35=TNativePin.PC2;  P36=TNativePin.PC1;  P37=TNativePin.PC3;
    P38=TNativePin.PC0;
    NONE=$ffffffff;
  end;

  TMorphoCN10Pin = record
  const
    P1 =TNativePin.PC9;  P2 =TNativePin.PC8;  P3 =TNativePin.PB8;  P4 =TNativePin.PC6;
    P5 =TNativePin.PB9;  P6 =TNativePin.PC5;  P10=TNativePin.PD8;  P11=TNativePin.PA5;
    P12=TNativePin.PA12; P13=TNativePin.PA6;  P14=TNativePin.PA11; P15=TNativePin.PA7;
    P16=TNativePin.PB12; P17=TNativePin.PB6;  P19=TNativePin.PC7;  P21=TNativePin.PA9;
    P22=TNativePin.PB2;  P23=TNativePin.PA8;  P24=TNativePin.PB1;  P25=TNativePin.PB10;
    P26=TNativePin.PB15; P27=TNativePin.PB4;  P28=TNativePin.PB14; P29=TNativePin.PB5;
    P30=TNativePin.PB13; P31=TNativePin.PB3;  P33=TNativePin.PA10; P34=TNativePin.PC4;
    P35=TNativePin.PA2;  P37=TNativePin.PA3;
    NONE=$ffffffff;
  end;
{$endif}

{$if defined(stm32f401xx)}
const
  MaxCPUFrequency=84000000;
{$elseif defined(stm32f411xx)}
const
  MaxCPUFrequency=100000000;
{$elseif defined(stm32f405xx) or defined(stm32f407xx) or defined(stm32f415xx) or defined(stm32f417xx)}
const
  MaxCPUFrequency=168000000;
{$elseif defined(stm32f427xx) or defined(stm32f437xx) or defined(stm32f429xx) or defined(stm32f439xx)
      or defined(stm32f446xx) or defined(stm32f469xx) or defined(stm32f479xx)}
const
  MaxCPUFrequency=180000000;
{$else}
  {$error Unknown Chip series, please define maximum CPU Frequency}
{$endif}
