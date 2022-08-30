{===============================================================================
  RzShellConsts Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  This unit declares the resource strings used for internationalisation of Shell
  Controls.

  To build your project with a single language's translations in the executable
  itself, declare one of the following conditional defines in your
  Project|Options. You will also then need to add the <RCx>\RzComps\Source path
  to your Search Path so this unit gets recompiled.
  
  To always use a specific language (without having to define a conditional
  symbol in your project options), then you can edit this file directly. After
  which you should run the !Build_RCx.cmd script in the <RCx>\RzComps\Source
  directory in order to rebuild the components and packages.
  

  Language             $DEFINE    2-character locale id
  -------------------  ---------- ---------------------
  Czech                LANG_CS    cs
  Danish               LANG_DA    da
  Dutch                LANG_NL    nl
  English              LANG_EN    en
  Finnish              LANG_FI    fi
  French               LANG_FR    fr
  German               LANG_DE    de
  Hungarian            LANG_HU    hu
  Italian              LANG_IT    it
  Japanese             LANG_JP    jp
  Norwegian            LANG_NO    no
  Polish               LANG_PL    pl
  Portuguese-Brazil    LANG_PTB   pt
  Portuguese-Portugal  LANG_PTP   pt
  Russian              LANG_RU    ru
  Spanish              LANG_ES    es
  Swedish              LANG_SV    sv


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

unit RzShellConsts;

interface

{$IFDEF LANG_CS}  {$I Lang\Czech\RzShellStrings.inc}                {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_DA}  {$I Lang\Danish\RzShellStrings.inc}               {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_DE}  {$I Lang\German\RzShellStrings.inc}               {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_ES}  {$I Lang\Spanish\RzShellStrings.inc}              {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_EN}  {$I Lang\English\RzShellStrings.inc}              {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_FI}  {$I Lang\Finnish\RzShellStrings.inc}              {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_FR}  {$I Lang\French\RzShellStrings.inc}               {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_HU}  {$I Lang\Hungarian\RzShellStrings.inc}            {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_IT}  {$I Lang\Italian\RzShellStrings.inc}              {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_JP}  {$I Lang\Japanese\RzShellStrings.inc}             {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_NL}  {$I Lang\Dutch\RzShellStrings.inc}                {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_NO}  {$I Lang\Norwegian\RzShellStrings.inc}            {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_PL}  {$I Lang\Polish\RzShellStrings.inc}               {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_PTB} {$I Lang\Portuguese_Brazil\RzShellStrings.inc}    {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_PTP} {$I Lang\Portuguese_Portugal\RzShellStrings.inc}  {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_RU}  {$I Lang\Russian\RzShellStrings.inc}              {$DEFINE APPLIED_LANG} {$ENDIF}
{$IFDEF LANG_SV}  {$I Lang\Swedish\RzShellStrings.inc}              {$DEFINE APPLIED_LANG} {$ENDIF}

{$IFNDEF APPLIED_LANG}
  {$I Lang\English\RzShellStrings.inc}    // Default to English
{$ENDIF}

implementation

end.

