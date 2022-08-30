{***************************************************************************}
{ XPTheme interface                                                         }
{ for Delphi & C++Builder                                                   }
{ version 1.0                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2002                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit ASXPVS;

{$HPPEMIT ''}
{$HPPEMIT '#include "uxtheme.h"'}
{$HPPEMIT ''}


interface

uses
  Windows, Graphics;

const

//---------------------------------------------------------------------------------------
//   "Window" (i.e., non-client) Parts & States
//---------------------------------------------------------------------------------------

  WP_CAPTION = 1;
  {$EXTERNALSYM WP_CAPTION}
  WP_SMALLCAPTION = 2;
  {$EXTERNALSYM WP_SMALLCAPTION}
  WP_MINCAPTION = 3;
  {$EXTERNALSYM WP_MINCAPTION}
  WP_SMALLMINCAPTION = 4;
  {$EXTERNALSYM WP_SMALLMINCAPTION}
  WP_MAXCAPTION = 5;
  {$EXTERNALSYM WP_MAXCAPTION}
  WP_SMALLMAXCAPTION = 6;
  {$EXTERNALSYM WP_SMALLMAXCAPTION}
  WP_FRAMELEFT = 7;
  {$EXTERNALSYM WP_FRAMELEFT}
  WP_FRAMERIGHT = 8;
  {$EXTERNALSYM WP_FRAMERIGHT}
  WP_FRAMEBOTTOM = 9;
  {$EXTERNALSYM WP_FRAMEBOTTOM}
  WP_SMALLFRAMELEFT = 10;
  {$EXTERNALSYM WP_SMALLFRAMELEFT}
  WP_SMALLFRAMERIGHT = 11;
  {$EXTERNALSYM WP_SMALLFRAMERIGHT}
  WP_SMALLFRAMEBOTTOM = 12;
  {$EXTERNALSYM WP_SMALLFRAMEBOTTOM}

    //---- window frame buttons ----
  WP_SYSBUTTON = 13;
  {$EXTERNALSYM WP_SYSBUTTON}
  WP_MDISYSBUTTON = 14;
  {$EXTERNALSYM WP_MDISYSBUTTON}
  WP_MINBUTTON = 15;
  {$EXTERNALSYM WP_MINBUTTON}
  
  WP_MDIMINBUTTON = 16;
  {$EXTERNALSYM WP_MDIMINBUTTON}
  WP_MAXBUTTON = 17;
  {$EXTERNALSYM WP_MAXBUTTON}
  WP_CLOSEBUTTON = 18;
  {$EXTERNALSYM WP_CLOSEBUTTON}
  WP_SMALLCLOSEBUTTON = 19;
  {$EXTERNALSYM WP_SMALLCLOSEBUTTON}
  WP_MDICLOSEBUTTON = 20;
  {$EXTERNALSYM WP_MDICLOSEBUTTON}
  WP_RESTOREBUTTON = 21;
  {$EXTERNALSYM WP_RESTOREBUTTON}
  WP_MDIRESTOREBUTTON = 22;
  {$EXTERNALSYM WP_MDIRESTOREBUTTON}
  WP_HELPBUTTON = 23;
  {$EXTERNALSYM WP_HELPBUTTON}
  WP_MDIHELPBUTTON = 24;
  {$EXTERNALSYM WP_MDIHELPBUTTON}
  //---- scrollbars
  WP_HORZSCROLL = 25;
  {$EXTERNALSYM WP_HORZSCROLL}
  WP_HORZTHUMB = 26;
  {$EXTERNALSYM WP_HORZTHUMB}
  WP_VERTSCROLL = 27;
  {$EXTERNALSYM WP_VERTSCROLL}
  WP_VERTTHUMB = 28;
  {$EXTERNALSYM WP_VERTTHUMB}
  //---- dialog ----
  WP_DIALOG = 29;
  {$EXTERNALSYM WP_DIALOG}
  //---- hit-test templates ---
  WP_CAPTIONSIZINGTEMPLATE = 30;
  {$EXTERNALSYM WP_CAPTIONSIZINGTEMPLATE}
  WP_SMALLCAPTIONSIZINGTEMPLATE = 31;
  {$EXTERNALSYM WP_SMALLCAPTIONSIZINGTEMPLATE}
  WP_FRAMELEFTSIZINGTEMPLATE = 32;
  {$EXTERNALSYM WP_FRAMELEFTSIZINGTEMPLATE}
  WP_SMALLFRAMELEFTSIZINGTEMPLATE = 33;
  {$EXTERNALSYM WP_SMALLFRAMELEFTSIZINGTEMPLATE}
  WP_FRAMERIGHTSIZINGTEMPLATE = 34;
  {$EXTERNALSYM WP_FRAMERIGHTSIZINGTEMPLATE}
  WP_SMALLFRAMERIGHTSIZINGTEMPLATE = 35;
  {$EXTERNALSYM WP_SMALLFRAMERIGHTSIZINGTEMPLATE}
  WP_FRAMEBOTTOMSIZINGTEMPLATE = 36;
  {$EXTERNALSYM WP_FRAMEBOTTOMSIZINGTEMPLATE}
  WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE = 37;
  {$EXTERNALSYM WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE}

  FS_ACTIVE = 1;
  {$EXTERNALSYM FS_ACTIVE}
  FS_INACTIVE = 2;
  {$EXTERNALSYM FS_INACTIVE}

  CS_ACTIVE = 1;
  {$EXTERNALSYM CS_ACTIVE}
  CS_INACTIVE = 2;
  {$EXTERNALSYM CS_INACTIVE}
  CS_DISABLED = 3;
  {$EXTERNALSYM CS_DISABLED}

  MXCS_ACTIVE = 1;
  {$EXTERNALSYM MXCS_ACTIVE}
  MXCS_INACTIVE = 2;
  {$EXTERNALSYM MXCS_INACTIVE}
  MXCS_DISABLED = 3;
  {$EXTERNALSYM MXCS_DISABLED}

  MNCS_ACTIVE = 1;
  {$EXTERNALSYM MNCS_ACTIVE}
  MNCS_INACTIVE = 2;
  {$EXTERNALSYM MNCS_INACTIVE}
  MNCS_DISABLED = 3;
  {$EXTERNALSYM MNCS_DISABLED}

  HSS_NORMAL = 1;
  {$EXTERNALSYM HSS_NORMAL}
  HSS_HOT = 2;
  {$EXTERNALSYM HSS_HOT}
  HSS_PUSHED = 3;
  {$EXTERNALSYM HSS_PUSHED}
  HSS_DISABLED = 4;
  {$EXTERNALSYM HSS_DISABLED}

  HTS_NORMAL = 1;
  {$EXTERNALSYM HTS_NORMAL}
  HTS_HOT = 2;
  {$EXTERNALSYM HTS_HOT}
  HTS_PUSHED = 3;
  {$EXTERNALSYM HTS_PUSHED}
  HTS_DISABLED = 4;
  {$EXTERNALSYM HTS_DISABLED}

  VSS_NORMAL = 1;
  {$EXTERNALSYM VSS_NORMAL}
  VSS_HOT = 2;
  {$EXTERNALSYM VSS_HOT}
  VSS_PUSHED = 3;
  {$EXTERNALSYM VSS_PUSHED}
  VSS_DISABLED = 4;
  {$EXTERNALSYM VSS_DISABLED}

  VTS_NORMAL = 1;
  {$EXTERNALSYM VTS_NORMAL}
  VTS_HOT = 2;
  {$EXTERNALSYM VTS_HOT}
  VTS_PUSHED = 3;
  {$EXTERNALSYM VTS_PUSHED}
  VTS_DISABLED = 4;
  {$EXTERNALSYM VTS_DISABLED}

  SBS_NORMAL = 1;
  {$EXTERNALSYM SBS_NORMAL}
  SBS_HOT = 2;
  {$EXTERNALSYM SBS_HOT}
  SBS_PUSHED = 3;
  {$EXTERNALSYM SBS_PUSHED}
  SBS_DISABLED = 4;
  {$EXTERNALSYM SBS_DISABLED}

  MINBS_NORMAL = 1;
  {$EXTERNALSYM MINBS_NORMAL}
  MINBS_HOT = 2;
  {$EXTERNALSYM MINBS_HOT}
  MINBS_PUSHED = 3;
  {$EXTERNALSYM MINBS_PUSHED}
  MINBS_DISABLED = 4;
  {$EXTERNALSYM MINBS_DISABLED}

  MAXBS_NORMAL = 1;
  {$EXTERNALSYM MAXBS_NORMAL}
  MAXBS_HOT = 2;
  {$EXTERNALSYM MAXBS_HOT}
  MAXBS_PUSHED = 3;
  {$EXTERNALSYM MAXBS_PUSHED}
  MAXBS_DISABLED = 4;
  {$EXTERNALSYM MAXBS_DISABLED}

  RBS_NORMAL = 1;
  {$EXTERNALSYM RBS_NORMAL}
  RBS_HOT = 2;
  {$EXTERNALSYM RBS_HOT}
  RBS_PUSHED = 3;
  {$EXTERNALSYM RBS_PUSHED}
  RBS_DISABLED = 4;
  {$EXTERNALSYM RBS_DISABLED}

  HBS_NORMAL = 1;
  {$EXTERNALSYM HBS_NORMAL}
  HBS_HOT = 2;
  {$EXTERNALSYM HBS_HOT}
  HBS_PUSHED = 3;
  {$EXTERNALSYM HBS_PUSHED}
  HBS_DISABLED = 4;
  {$EXTERNALSYM HBS_DISABLED}

  CBS_NORMAL = 1;
  {$EXTERNALSYM CBS_NORMAL}
  CBS_HOT = 2;
  {$EXTERNALSYM CBS_HOT}
  CBS_PUSHED = 3;
  {$EXTERNALSYM CBS_PUSHED}
  CBS_DISABLED = 4;
  {$EXTERNALSYM CBS_DISABLED}

//---------------------------------------------------------------------------------------
//   "Button" Parts & States
//---------------------------------------------------------------------------------------
  BP_PUSHBUTTON = 1;
  {$EXTERNALSYM BP_PUSHBUTTON}
  BP_RADIOBUTTON = 2;
  {$EXTERNALSYM BP_RADIOBUTTON}
  BP_CHECKBOX = 3;
  {$EXTERNALSYM BP_CHECKBOX}
  BP_GROUPBOX = 4;
  {$EXTERNALSYM BP_GROUPBOX}
  BP_USERBUTTON = 5;
  {$EXTERNALSYM BP_USERBUTTON}

  PBS_NORMAL = 1;
  {$EXTERNALSYM PBS_NORMAL}
  PBS_HOT = 2;
  {$EXTERNALSYM PBS_HOT}
  PBS_PRESSED = 3;
  {$EXTERNALSYM PBS_PRESSED}
  PBS_DISABLED = 4;
  {$EXTERNALSYM PBS_DISABLED}
  PBS_DEFAULTED = 5;
  {$EXTERNALSYM PBS_DEFAULTED}

  RBS_UNCHECKEDNORMAL = 1;
  {$EXTERNALSYM RBS_UNCHECKEDNORMAL}
  RBS_UNCHECKEDHOT = 2;
  {$EXTERNALSYM RBS_UNCHECKEDHOT}
  RBS_UNCHECKEDPRESSED = 3;
  {$EXTERNALSYM RBS_UNCHECKEDPRESSED}
  RBS_UNCHECKEDDISABLED = 4;
  {$EXTERNALSYM RBS_UNCHECKEDDISABLED}
  RBS_CHECKEDNORMAL = 5;
  {$EXTERNALSYM RBS_CHECKEDNORMAL}
  RBS_CHECKEDHOT = 6;
  {$EXTERNALSYM RBS_CHECKEDHOT}
  RBS_CHECKEDPRESSED = 7;
  {$EXTERNALSYM RBS_CHECKEDPRESSED}
  RBS_CHECKEDDISABLED = 8;
  {$EXTERNALSYM RBS_CHECKEDDISABLED}

  CBS_UNCHECKEDNORMAL = 1;
  {$EXTERNALSYM CBS_UNCHECKEDNORMAL}
  CBS_UNCHECKEDHOT = 2;
  {$EXTERNALSYM CBS_UNCHECKEDHOT}
  CBS_UNCHECKEDPRESSED = 3;
  {$EXTERNALSYM CBS_UNCHECKEDPRESSED}
  CBS_UNCHECKEDDISABLED = 4;
  {$EXTERNALSYM CBS_UNCHECKEDDISABLED}
  CBS_CHECKEDNORMAL = 5;
  {$EXTERNALSYM CBS_CHECKEDNORMAL}
  CBS_CHECKEDHOT = 6;
  {$EXTERNALSYM CBS_CHECKEDHOT}
  CBS_CHECKEDPRESSED = 7;
  {$EXTERNALSYM CBS_CHECKEDPRESSED}
  CBS_CHECKEDDISABLED = 8;
  {$EXTERNALSYM CBS_CHECKEDDISABLED}
  CBS_MIXEDNORMAL = 9;
  {$EXTERNALSYM CBS_MIXEDNORMAL}
  CBS_MIXEDHOT = 10;
  {$EXTERNALSYM CBS_MIXEDHOT}
  CBS_MIXEDPRESSED = 11;
  {$EXTERNALSYM CBS_MIXEDPRESSED}
  CBS_MIXEDDISABLED = 12;
  {$EXTERNALSYM CBS_MIXEDDISABLED}

  GBS_NORMAL = 1;
  {$EXTERNALSYM GBS_NORMAL}
  GBS_DISABLED = 2;
  {$EXTERNALSYM GBS_DISABLED}

//---------------------------------------------------------------------------------------
//   "Rebar" Parts & States
//---------------------------------------------------------------------------------------

  RP_GRIPPER = 1;
  {$EXTERNALSYM RP_GRIPPER}
  RP_GRIPPERVERT = 2;
  {$EXTERNALSYM RP_GRIPPERVERT}
  RP_BAND = 3;
  {$EXTERNALSYM RP_BAND}
  RP_CHEVRON = 4;
  {$EXTERNALSYM RP_CHEVRON}
  RP_CHEVRONVERT = 5;
  {$EXTERNALSYM RP_CHEVRONVERT}

  CHEVS_NORMAL = 1;
  {$EXTERNALSYM CHEVS_NORMAL}
  CHEVS_HOT = 2;
  {$EXTERNALSYM CHEVS_HOT}
  CHEVS_PRESSED = 3;
  {$EXTERNALSYM CHEVS_PRESSED}

//---------------------------------------------------------------------------------------
//   "Toolbar" Parts & States
//---------------------------------------------------------------------------------------

  TP_BUTTON = 1;
  {$EXTERNALSYM TP_BUTTON}
  TP_DROPDOWNBUTTON = 2;
  {$EXTERNALSYM TP_DROPDOWNBUTTON}
  TP_SPLITBUTTON = 3;
  {$EXTERNALSYM TP_SPLITBUTTON}
  TP_SPLITBUTTONDROPDOWN = 4;
  {$EXTERNALSYM TP_SPLITBUTTONDROPDOWN}
  TP_SEPARATOR = 5;
  {$EXTERNALSYM TP_SEPARATOR}
  TP_SEPARATORVERT = 6;
  {$EXTERNALSYM TP_SEPARATORVERT}

  TS_NORMAL = 1;
  {$EXTERNALSYM TS_NORMAL}
  TS_HOT = 2;
  {$EXTERNALSYM TS_HOT}
  TS_PRESSED = 3;
  {$EXTERNALSYM TS_PRESSED}
  TS_DISABLED = 4;
  {$EXTERNALSYM TS_DISABLED}
  TS_CHECKED = 5;
  {$EXTERNALSYM TS_CHECKED}
  TS_HOTCHECKED = 6;
  {$EXTERNALSYM TS_HOTCHECKED}

//---------------------------------------------------------------------------------------
//   "Status" Parts & States
//---------------------------------------------------------------------------------------
  SP_PANE = 1;
  {$EXTERNALSYM SP_PANE}
  SP_GRIPPERPANE = 2;
  {$EXTERNALSYM SP_GRIPPERPANE}
  SP_GRIPPER = 3;
  {$EXTERNALSYM SP_GRIPPER}

//---------------------------------------------------------------------------------------
//   "Menu" Parts & States
//---------------------------------------------------------------------------------------

  MP_MENUITEM = 1;
  {$EXTERNALSYM MP_MENUITEM}
  MP_MENUDROPDOWN = 2;
  {$EXTERNALSYM MP_MENUDROPDOWN}
  MP_MENUBARITEM = 3;
  {$EXTERNALSYM MP_MENUBARITEM}
  MP_MENUBARDROPDOWN = 4;
  {$EXTERNALSYM MP_MENUBARDROPDOWN}
  MP_CHEVRON = 5;
  {$EXTERNALSYM MP_CHEVRON}
  MP_SEPARATOR = 6;
  {$EXTERNALSYM MP_SEPARATOR}

  MS_NORMAL = 1;
  {$EXTERNALSYM MS_NORMAL}
  MS_SELECTED = 2;
  {$EXTERNALSYM MS_SELECTED}
  MS_DEMOTED = 3;
  {$EXTERNALSYM MS_DEMOTED}

//---------------------------------------------------------------------------------------
//   "ListView" Parts & States
//---------------------------------------------------------------------------------------

  LVP_LISTITEM = 1;
  {$EXTERNALSYM LVP_LISTITEM}
  LVP_LISTGROUP = 2;
  {$EXTERNALSYM LVP_LISTGROUP}
  LVP_LISTDETAIL = 3;
  {$EXTERNALSYM LVP_LISTDETAIL}
  LVP_LISTSORTEDDETAIL = 4;
  {$EXTERNALSYM LVP_LISTSORTEDDETAIL}
  LVP_EMPTYTEXT = 5;
  {$EXTERNALSYM LVP_EMPTYTEXT}

  LIS_NORMAL = 1;
  {$EXTERNALSYM LIS_NORMAL}
  LIS_HOT = 2;
  {$EXTERNALSYM LIS_HOT}
  LIS_SELECTED = 3;
  {$EXTERNALSYM LIS_SELECTED}
  LIS_DISABLED = 4;
  {$EXTERNALSYM LIS_DISABLED}
  LIS_SELECTEDNOTFOCUS = 5;
  {$EXTERNALSYM LIS_SELECTEDNOTFOCUS}

//---------------------------------------------------------------------------------------
//   "Header" Parts & States
//---------------------------------------------------------------------------------------
  HP_HEADERITEM = 1;
  {$EXTERNALSYM HP_HEADERITEM}
  HP_HEADERITEMLEFT = 2;
  {$EXTERNALSYM HP_HEADERITEMLEFT}
  HP_HEADERITEMRIGHT = 3;
  {$EXTERNALSYM HP_HEADERITEMRIGHT}
  HP_HEADERSORTARROW = 4;
  {$EXTERNALSYM HP_HEADERSORTARROW}

  HIS_NORMAL = 1;
  {$EXTERNALSYM HIS_NORMAL}
  HIS_HOT = 2;
  {$EXTERNALSYM HIS_HOT}
  HIS_PRESSED = 3;
  {$EXTERNALSYM HIS_PRESSED}

  HILS_NORMAL = 1;
  {$EXTERNALSYM HILS_NORMAL}
  HILS_HOT = 2;
  {$EXTERNALSYM HILS_HOT}
  HILS_PRESSED = 3;
  {$EXTERNALSYM HILS_PRESSED}

  HIRS_NORMAL = 1;
  {$EXTERNALSYM HIRS_NORMAL}
  HIRS_HOT = 2;
  {$EXTERNALSYM HIRS_HOT}
  HIRS_PRESSED = 3;
  {$EXTERNALSYM HIRS_PRESSED}

  HSAS_SORTEDUP = 1;
  {$EXTERNALSYM HSAS_SORTEDUP}
  HSAS_SORTEDDOWN = 2;
  {$EXTERNALSYM HSAS_SORTEDDOWN}

//---------------------------------------------------------------------------------------
//   "Progress" Parts & States
//---------------------------------------------------------------------------------------
  PP_BAR = 1;
  {$EXTERNALSYM PP_BAR}
  PP_BARVERT = 2;
  {$EXTERNALSYM PP_BARVERT}
  PP_CHUNK = 3;
  {$EXTERNALSYM PP_CHUNK}
  PP_CHUNKVERT = 4;
  {$EXTERNALSYM PP_CHUNKVERT}

//---------------------------------------------------------------------------------------
//   "Tab" Parts & States
//---------------------------------------------------------------------------------------

  TABP_TABITEM = 1;
  {$EXTERNALSYM TABP_TABITEM}
  TABP_TABITEMLEFTEDGE = 2;
  {$EXTERNALSYM TABP_TABITEMLEFTEDGE}
  TABP_TABITEMRIGHTEDGE = 3;
  {$EXTERNALSYM TABP_TABITEMRIGHTEDGE}
  TABP_TABITEMBOTHEDGE = 4;
  {$EXTERNALSYM TABP_TABITEMBOTHEDGE}
  TABP_TOPTABITEM = 5;
  {$EXTERNALSYM TABP_TOPTABITEM}
  TABP_TOPTABITEMLEFTEDGE = 6;
  {$EXTERNALSYM TABP_TOPTABITEMLEFTEDGE}
  TABP_TOPTABITEMRIGHTEDGE = 7;
  {$EXTERNALSYM TABP_TOPTABITEMRIGHTEDGE}
  TABP_TOPTABITEMBOTHEDGE = 8;
  {$EXTERNALSYM TABP_TOPTABITEMBOTHEDGE}
  TABP_PANE = 9;
  {$EXTERNALSYM TABP_PANE}
  TABP_BODY = 10;
  {$EXTERNALSYM TABP_BODY}

  TIS_NORMAL = 1;
  {$EXTERNALSYM TIS_NORMAL}
  TIS_HOT = 2;
  {$EXTERNALSYM TIS_HOT}
  TIS_SELECTED = 3;
  {$EXTERNALSYM TIS_SELECTED}
  TIS_DISABLED = 4;
  {$EXTERNALSYM TIS_DISABLED}
  TIS_FOCUSED = 5;
  {$EXTERNALSYM TIS_FOCUSED}

  TILES_NORMAL = 1;
  {$EXTERNALSYM TILES_NORMAL}
  TILES_HOT = 2;
  {$EXTERNALSYM TILES_HOT}
  TILES_SELECTED = 3;
  {$EXTERNALSYM TILES_SELECTED}
  TILES_DISABLED = 4;
  {$EXTERNALSYM TILES_DISABLED}
  TILES_FOCUSED = 5;
  {$EXTERNALSYM TILES_FOCUSED}

  TIRES_NORMAL = 1;
  {$EXTERNALSYM TIRES_NORMAL}
  TIRES_HOT = 2;
  {$EXTERNALSYM TIRES_HOT}
  TIRES_SELECTED = 3;
  {$EXTERNALSYM TIRES_SELECTED}
  TIRES_DISABLED = 4;
  {$EXTERNALSYM TIRES_DISABLED}
  TIRES_FOCUSED = 5;
  {$EXTERNALSYM TIRES_FOCUSED}

  TIBES_NORMAL = 1;
  {$EXTERNALSYM TIBES_NORMAL}
  TIBES_HOT = 2;
  {$EXTERNALSYM TIBES_HOT}
  TIBES_SELECTED = 3;
  {$EXTERNALSYM TIBES_SELECTED}
  TIBES_DISABLED = 4;
  {$EXTERNALSYM TIBES_DISABLED}
  TIBES_FOCUSED = 5;
  {$EXTERNALSYM TIBES_FOCUSED}

  TTIS_NORMAL = 1;
  {$EXTERNALSYM TTIS_NORMAL}
  TTIS_HOT = 2;
  {$EXTERNALSYM TTIS_HOT}
  TTIS_SELECTED = 3;
  {$EXTERNALSYM TTIS_SELECTED}
  TTIS_DISABLED = 4;
  {$EXTERNALSYM TTIS_DISABLED}
  TTIS_FOCUSED = 5;
  {$EXTERNALSYM TTIS_FOCUSED}

  TTILES_NORMAL = 1;
  {$EXTERNALSYM TTILES_NORMAL}
  TTILES_HOT = 2;
  {$EXTERNALSYM TTILES_HOT}
  TTILES_SELECTED = 3;
  {$EXTERNALSYM TTILES_SELECTED}
  TTILES_DISABLED = 4;
  {$EXTERNALSYM TTILES_DISABLED}
  TTILES_FOCUSED = 5;
  {$EXTERNALSYM TTILES_FOCUSED}

  TTIRES_NORMAL = 1;
  {$EXTERNALSYM TTIRES_NORMAL}
  TTIRES_HOT = 2;
  {$EXTERNALSYM TTIRES_HOT}
  TTIRES_SELECTED = 3;
  {$EXTERNALSYM TTIRES_SELECTED}
  TTIRES_DISABLED = 4;
  {$EXTERNALSYM TTIRES_DISABLED}
  TTIRES_FOCUSED = 5;
  {$EXTERNALSYM TTIRES_FOCUSED}

  TTIBES_NORMAL = 1;
  {$EXTERNALSYM TTIBES_NORMAL}
  TTIBES_HOT = 2;
  {$EXTERNALSYM TTIBES_HOT}
  TTIBES_SELECTED = 3;
  {$EXTERNALSYM TTIBES_SELECTED}
  TTIBES_DISABLED = 4;
  {$EXTERNALSYM TTIBES_DISABLED}
  TTIBES_FOCUSED = 5;
  {$EXTERNALSYM TTIBES_FOCUSED}

//---------------------------------------------------------------------------------------
//   "Trackbar" Parts & States
//---------------------------------------------------------------------------------------

  TKP_TRACK = 1;
  {$EXTERNALSYM TKP_TRACK}
  TKP_TRACKVERT = 2;
  {$EXTERNALSYM TKP_TRACKVERT}
  TKP_THUMB = 3;
  {$EXTERNALSYM TKP_THUMB}
  TKP_THUMBBOTTOM = 4;
  {$EXTERNALSYM TKP_THUMBBOTTOM}
  TKP_THUMBTOP = 5;
  {$EXTERNALSYM TKP_THUMBTOP}
  TKP_THUMBVERT = 6;
  {$EXTERNALSYM TKP_THUMBVERT}
  TKP_THUMBLEFT = 7;
  {$EXTERNALSYM TKP_THUMBLEFT}
  TKP_THUMBRIGHT = 8;
  {$EXTERNALSYM TKP_THUMBRIGHT}
  TKP_TICS = 9;
  {$EXTERNALSYM TKP_TICS}
  TKP_TICSVERT = 10;
  {$EXTERNALSYM TKP_TICSVERT}

  TKS_NORMAL = 1;
  {$EXTERNALSYM TKS_NORMAL}
  TRS_NORMAL = 1;
  {$EXTERNALSYM TRS_NORMAL}
  TRVS_NORMAL = 1;
  {$EXTERNALSYM TRVS_NORMAL}

  TUS_NORMAL = 1;
  {$EXTERNALSYM TUS_NORMAL}
  TUS_HOT = 2;
  {$EXTERNALSYM TUS_HOT}
  TUS_PRESSED = 3;
  {$EXTERNALSYM TUS_PRESSED}
  TUS_FOCUSED = 4;
  {$EXTERNALSYM TUS_FOCUSED}
  TUS_DISABLED = 5;
  {$EXTERNALSYM TUS_DISABLED}

  TUBS_NORMAL = 1;
  {$EXTERNALSYM TUBS_NORMAL}
  TUBS_HOT = 2;
  {$EXTERNALSYM TUBS_HOT}
  TUBS_PRESSED = 3;
  {$EXTERNALSYM TUBS_PRESSED}
  TUBS_FOCUSED = 4;
  {$EXTERNALSYM TUBS_FOCUSED}
  TUBS_DISABLED = 5;
  {$EXTERNALSYM TUBS_DISABLED}

  TUTS_NORMAL = 1;
  {$EXTERNALSYM TUTS_NORMAL}
  TUTS_HOT = 2;
  {$EXTERNALSYM TUTS_HOT}
  TUTS_PRESSED = 3;
  {$EXTERNALSYM TUTS_PRESSED}
  TUTS_FOCUSED = 4;
  {$EXTERNALSYM TUTS_FOCUSED}
  TUTS_DISABLED = 5;
  {$EXTERNALSYM TUTS_DISABLED}

  TUVS_NORMAL = 1;
  {$EXTERNALSYM TUVS_NORMAL}
  TUVS_HOT = 2;
  {$EXTERNALSYM TUVS_HOT}
  TUVS_PRESSED = 3;
  {$EXTERNALSYM TUVS_PRESSED}
  TUVS_FOCUSED = 4;
  {$EXTERNALSYM TUVS_FOCUSED}
  TUVS_DISABLED = 5;
  {$EXTERNALSYM TUVS_DISABLED}

  TUVLS_NORMAL = 1;
  {$EXTERNALSYM TUVLS_NORMAL}
  TUVLS_HOT = 2;
  {$EXTERNALSYM TUVLS_HOT}
  TUVLS_PRESSED = 3;
  {$EXTERNALSYM TUVLS_PRESSED}
  TUVLS_FOCUSED = 4;
  {$EXTERNALSYM TUVLS_FOCUSED}
  TUVLS_DISABLED = 5;
  {$EXTERNALSYM TUVLS_DISABLED}

  TUVRS_NORMAL = 1;
  {$EXTERNALSYM TUVRS_NORMAL}
  TUVRS_HOT = 2;
  {$EXTERNALSYM TUVRS_HOT}
  TUVRS_PRESSED = 3;
  {$EXTERNALSYM TUVRS_PRESSED}
  TUVRS_FOCUSED = 4;
  {$EXTERNALSYM TUVRS_FOCUSED}
  TUVRS_DISABLED = 5;
  {$EXTERNALSYM TUVRS_DISABLED}

  TSS_NORMAL = 1;
  {$EXTERNALSYM TSS_NORMAL}

  TSVS_NORMAL = 1;
  {$EXTERNALSYM TSVS_NORMAL}

//---------------------------------------------------------------------------------------
//   "Tooltips" Parts & States
//---------------------------------------------------------------------------------------

  TTP_STANDARD = 1;
  {$EXTERNALSYM TTP_STANDARD}
  TTP_STANDARDTITLE = 2;
  {$EXTERNALSYM TTP_STANDARDTITLE}
  TTP_BALLOON = 3;
  {$EXTERNALSYM TTP_BALLOON}
  TTP_BALLOONTITLE = 4;
  {$EXTERNALSYM TTP_BALLOONTITLE}
  TTP_CLOSE = 5;
  {$EXTERNALSYM TTP_CLOSE}

  TTCS_NORMAL = 1;
  {$EXTERNALSYM TTCS_NORMAL}
  TTCS_HOT = 2;
  {$EXTERNALSYM TTCS_HOT}
  TTCS_PRESSED = 3;
  {$EXTERNALSYM TTCS_PRESSED}

  TTSS_NORMAL = 1;
  {$EXTERNALSYM TTSS_NORMAL}
  TTSS_LINK = 2;
  {$EXTERNALSYM TTSS_LINK}

  TTBS_NORMAL = 1;
  {$EXTERNALSYM TTBS_NORMAL}
  TTBS_LINK = 2;
  {$EXTERNALSYM TTBS_LINK}

//---------------------------------------------------------------------------------------
//   "TreeView" Parts & States
//---------------------------------------------------------------------------------------

  TVP_TREEITEM = 1;
  {$EXTERNALSYM TVP_TREEITEM}
  TVP_GLYPH = 2;
  {$EXTERNALSYM TVP_GLYPH}
  TVP_BRANCH = 3;
  {$EXTERNALSYM TVP_BRANCH}

  TREIS_NORMAL = 1;
  {$EXTERNALSYM TREIS_NORMAL}
  TREIS_HOT = 2;
  {$EXTERNALSYM TREIS_HOT}
  TREIS_SELECTED = 3;
  {$EXTERNALSYM TREIS_SELECTED}
  TREIS_DISABLED = 4;
  {$EXTERNALSYM TREIS_DISABLED}
  TREIS_SELECTEDNOTFOCUS = 5;
  {$EXTERNALSYM TREIS_SELECTEDNOTFOCUS}

  GLPS_CLOSED = 1;
  {$EXTERNALSYM GLPS_CLOSED}
  GLPS_OPENED = 2;
  {$EXTERNALSYM GLPS_OPENED}

//---------------------------------------------------------------------------------------
//   "Spin" Parts & States
//---------------------------------------------------------------------------------------

  SPNP_UP = 1;
  {$EXTERNALSYM SPNP_UP}
  SPNP_DOWN = 2;
  {$EXTERNALSYM SPNP_DOWN}
  SPNP_UPHORZ = 3;
  {$EXTERNALSYM SPNP_UPHORZ}
  SPNP_DOWNHORZ = 4;
  {$EXTERNALSYM SPNP_DOWNHORZ}

  UPS_NORMAL = 1;
  {$EXTERNALSYM UPS_NORMAL}
  UPS_HOT = 2;
  {$EXTERNALSYM UPS_HOT}
  UPS_PRESSED = 3;
  {$EXTERNALSYM UPS_PRESSED}
  UPS_DISABLED = 4;
  {$EXTERNALSYM UPS_DISABLED}

  DNS_NORMAL = 1;
  {$EXTERNALSYM DNS_NORMAL}
  DNS_HOT = 2;
  {$EXTERNALSYM DNS_HOT}
  DNS_PRESSED = 3;
  {$EXTERNALSYM DNS_PRESSED}
  DNS_DISABLED = 4;
  {$EXTERNALSYM DNS_DISABLED}

  UPHZS_NORMAL = 1;
  {$EXTERNALSYM UPHZS_NORMAL}
  UPHZS_HOT = 2;
  {$EXTERNALSYM UPHZS_HOT}
  UPHZS_PRESSED = 3;
  {$EXTERNALSYM UPHZS_PRESSED}
  UPHZS_DISABLED = 4;
  {$EXTERNALSYM UPHZS_DISABLED}

  DNHZS_NORMAL = 1;
  {$EXTERNALSYM DNHZS_NORMAL}
  DNHZS_HOT = 2;
  {$EXTERNALSYM DNHZS_HOT}
  DNHZS_PRESSED = 3;
  {$EXTERNALSYM DNHZS_PRESSED}
  DNHZS_DISABLED = 4;
  {$EXTERNALSYM DNHZS_DISABLED}

//---------------------------------------------------------------------------------------
//   "Page" Parts & States
//---------------------------------------------------------------------------------------

  PGRP_UP = 1;
  {$EXTERNALSYM PGRP_UP}
  PGRP_DOWN = 2;
  {$EXTERNALSYM PGRP_DOWN}
  PGRP_UPHORZ = 3;
  {$EXTERNALSYM PGRP_UPHORZ}
  PGRP_DOWNHORZ = 4;
  {$EXTERNALSYM PGRP_DOWNHORZ}

//--- Pager uses same states as Spin ---

//---------------------------------------------------------------------------------------
//   "Scrollbar" Parts & States
//---------------------------------------------------------------------------------------

  SBP_ARROWBTN = 1;
  {$EXTERNALSYM SBP_ARROWBTN}
  SBP_THUMBBTNHORZ = 2;
  {$EXTERNALSYM SBP_THUMBBTNHORZ}
  SBP_THUMBBTNVERT = 3;
  {$EXTERNALSYM SBP_THUMBBTNVERT}
  SBP_LOWERTRACKHORZ = 4;
  {$EXTERNALSYM SBP_LOWERTRACKHORZ}
  SBP_UPPERTRACKHORZ = 5;
  {$EXTERNALSYM SBP_UPPERTRACKHORZ}
  SBP_LOWERTRACKVERT = 6;
  {$EXTERNALSYM SBP_LOWERTRACKVERT}
  SBP_UPPERTRACKVERT = 7;
  {$EXTERNALSYM SBP_UPPERTRACKVERT}
  SBP_GRIPPERHORZ = 8;
  {$EXTERNALSYM SBP_GRIPPERHORZ}
  SBP_GRIPPERVERT = 9;
  {$EXTERNALSYM SBP_GRIPPERVERT}
  SBP_SIZEBOX = 10;
  {$EXTERNALSYM SBP_SIZEBOX}

  ABS_UPNORMAL = 1;
  {$EXTERNALSYM ABS_UPNORMAL}
  ABS_UPHOT = 2;
  {$EXTERNALSYM ABS_UPHOT}
  ABS_UPPRESSED = 3;
  {$EXTERNALSYM ABS_UPPRESSED}
  ABS_UPDISABLED = 4;
  {$EXTERNALSYM ABS_UPDISABLED}
  ABS_DOWNNORMAL = 5;
  {$EXTERNALSYM ABS_DOWNNORMAL}
  ABS_DOWNHOT = 6;
  {$EXTERNALSYM ABS_DOWNHOT}
  ABS_DOWNPRESSED = 7;
  {$EXTERNALSYM ABS_DOWNPRESSED}
  ABS_DOWNDISABLED = 8;
  {$EXTERNALSYM ABS_DOWNDISABLED}
  ABS_LEFTNORMAL = 9;
  {$EXTERNALSYM ABS_LEFTNORMAL}
  ABS_LEFTHOT = 10;
  {$EXTERNALSYM ABS_LEFTHOT}
  ABS_LEFTPRESSED = 11;
  {$EXTERNALSYM ABS_LEFTPRESSED}
  ABS_LEFTDISABLED = 12;
  {$EXTERNALSYM ABS_LEFTDISABLED}
  ABS_RIGHTNORMAL = 13;
  {$EXTERNALSYM ABS_RIGHTNORMAL}
  ABS_RIGHTHOT = 14;
  {$EXTERNALSYM ABS_RIGHTHOT}
  ABS_RIGHTPRESSED = 15;
  {$EXTERNALSYM ABS_RIGHTPRESSED}
  ABS_RIGHTDISABLED = 16;
  {$EXTERNALSYM ABS_RIGHTDISABLED}

  SCRBS_NORMAL = 1;
  {$EXTERNALSYM SCRBS_NORMAL}
  SCRBS_HOT = 2;
  {$EXTERNALSYM SCRBS_HOT}
  SCRBS_PRESSED = 3;
  {$EXTERNALSYM SCRBS_PRESSED}
  SCRBS_DISABLED = 4;
  {$EXTERNALSYM SCRBS_DISABLED}

  SZB_RIGHTALIGN = 1;
  {$EXTERNALSYM SZB_RIGHTALIGN}
  SZB_LEFTALIGN = 2;
  {$EXTERNALSYM SZB_LEFTALIGN}

//---------------------------------------------------------------------------------------
//   "Edit" Parts & States
//---------------------------------------------------------------------------------------
  EP_EDITTEXT = 1;
  {$EXTERNALSYM EP_EDITTEXT}
  EP_CARET = 2;
  {$EXTERNALSYM EP_CARET}

  ETS_NORMAL = 1;
  {$EXTERNALSYM ETS_NORMAL}
  ETS_HOT = 2;
  {$EXTERNALSYM ETS_HOT}
  ETS_SELECTED = 3;
  {$EXTERNALSYM ETS_SELECTED}
  ETS_DISABLED = 4;
  {$EXTERNALSYM ETS_DISABLED}
  ETS_FOCUSED = 5;
  {$EXTERNALSYM ETS_FOCUSED}
  ETS_READONLY = 6;
  {$EXTERNALSYM ETS_READONLY}
  ETS_ASSIST = 7;
  {$EXTERNALSYM ETS_ASSIST}

//---------------------------------------------------------------------------------------
//   "ComboBox" Parts & States
//---------------------------------------------------------------------------------------

  CP_DROPDOWNBUTTON = 1;
  {$EXTERNALSYM CP_DROPDOWNBUTTON}

  CBXS_NORMAL = 1;
  {$EXTERNALSYM CBXS_NORMAL}
  CBXS_HOT = 2;
  {$EXTERNALSYM CBXS_HOT}
  CBXS_PRESSED = 3;
  {$EXTERNALSYM CBXS_PRESSED}
  CBXS_DISABLED = 4;
  {$EXTERNALSYM CBXS_DISABLED}

//---------------------------------------------------------------------------------------
//   "Taskbar Clock" Parts & States
//---------------------------------------------------------------------------------------

  CLP_TIME = 1;
  {$EXTERNALSYM CLP_TIME}
  CLS_NORMAL = 1;
  {$EXTERNALSYM CLS_NORMAL}

//---------------------------------------------------------------------------------------
//   "Tray Notify" Parts & States
//---------------------------------------------------------------------------------------

  TNP_BACKGROUND = 1;
  {$EXTERNALSYM TNP_BACKGROUND}
  TNP_ANIMBACKGROUND = 2;
  {$EXTERNALSYM TNP_ANIMBACKGROUND}

//---------------------------------------------------------------------------------------
//   "TaskBar" Parts & States
//---------------------------------------------------------------------------------------

  TBP_BACKGROUNDBOTTOM = 1;
  {$EXTERNALSYM TBP_BACKGROUNDBOTTOM}
  TBP_BACKGROUNDRIGHT = 2;
  {$EXTERNALSYM TBP_BACKGROUNDRIGHT}
  TBP_BACKGROUNDTOP = 3;
  {$EXTERNALSYM TBP_BACKGROUNDTOP}
  TBP_BACKGROUNDLEFT = 4;
  {$EXTERNALSYM TBP_BACKGROUNDLEFT}
  TBP_SIZINGBARBOTTOM = 5;
  {$EXTERNALSYM TBP_SIZINGBARBOTTOM}
  TBP_SIZINGBARRIGHT = 6;
  {$EXTERNALSYM TBP_SIZINGBARRIGHT}
  TBP_SIZINGBARTOP = 7;
  {$EXTERNALSYM TBP_SIZINGBARTOP}
  TBP_SIZINGBARLEFT = 8;
  {$EXTERNALSYM TBP_SIZINGBARLEFT}

//---------------------------------------------------------------------------------------
//   "TaskBand" Parts & States
//---------------------------------------------------------------------------------------

  TDP_GROUPCOUNT = 1;
  {$EXTERNALSYM TDP_GROUPCOUNT}
  TDP_FLASHBUTTON = 2;
  {$EXTERNALSYM TDP_FLASHBUTTON}
  TDP_FLASHBUTTONGROUPMENU = 3;
  {$EXTERNALSYM TDP_FLASHBUTTONGROUPMENU}

//---------------------------------------------------------------------------------------
//   "StartPanel" Parts & States
//---------------------------------------------------------------------------------------

  SPP_USERPANE = 1;
  {$EXTERNALSYM SPP_USERPANE}
  SPP_MOREPROGRAMS = 2;
  {$EXTERNALSYM SPP_MOREPROGRAMS}
  SPP_MOREPROGRAMSARROW = 3;
  {$EXTERNALSYM SPP_MOREPROGRAMSARROW}
  SPP_PROGLIST = 4;
  {$EXTERNALSYM SPP_PROGLIST}
  SPP_PROGLISTSEPARATOR = 5;
  {$EXTERNALSYM SPP_PROGLISTSEPARATOR}
  SPP_PLACESLIST = 6;
  {$EXTERNALSYM SPP_PLACESLIST}
  SPP_PLACESLISTSEPARATOR = 7;
  {$EXTERNALSYM SPP_PLACESLISTSEPARATOR}
  SPP_LOGOFF = 8;
  {$EXTERNALSYM SPP_LOGOFF}
  SPP_LOGOFFBUTTONS = 9;
  {$EXTERNALSYM SPP_LOGOFFBUTTONS}
  SPP_USERPICTURE = 10;
  {$EXTERNALSYM SPP_USERPICTURE}
  SPP_PREVIEW = 11;
  {$EXTERNALSYM SPP_PREVIEW}

  SPS_NORMAL = 1;
  {$EXTERNALSYM SPS_NORMAL}
  SPS_HOT = 2;
  {$EXTERNALSYM SPS_HOT}
  SPS_PRESSED = 3;
  {$EXTERNALSYM SPS_PRESSED}

  SPLS_NORMAL = 1;
  {$EXTERNALSYM SPLS_NORMAL}
  SPLS_HOT = 2;
  {$EXTERNALSYM SPLS_HOT}
  SPLS_PRESSED = 3;
  {$EXTERNALSYM SPLS_PRESSED}

//---------------------------------------------------------------------------------------
//   "ExplorerBar" Parts & States
//---------------------------------------------------------------------------------------

  EBP_HEADERBACKGROUND = 1;
  {$EXTERNALSYM EBP_HEADERBACKGROUND}
  EBP_HEADERCLOSE = 2;
  {$EXTERNALSYM EBP_HEADERCLOSE}
  EBP_HEADERPIN = 3;
  {$EXTERNALSYM EBP_HEADERPIN}
  EBP_IEBARMENU = 4;
  {$EXTERNALSYM EBP_IEBARMENU}
  EBP_NORMALGROUPBACKGROUND = 5;
  {$EXTERNALSYM EBP_NORMALGROUPBACKGROUND}
  EBP_NORMALGROUPCOLLAPSE = 6;
  {$EXTERNALSYM EBP_NORMALGROUPCOLLAPSE}
  EBP_NORMALGROUPEXPAND = 7;
  {$EXTERNALSYM EBP_NORMALGROUPEXPAND}
  EBP_NORMALGROUPHEAD = 8;
  {$EXTERNALSYM EBP_NORMALGROUPHEAD}
  EBP_SPECIALGROUPBACKGROUND = 9;
  {$EXTERNALSYM EBP_SPECIALGROUPBACKGROUND}
  EBP_SPECIALGROUPCOLLAPSE = 10;
  {$EXTERNALSYM EBP_SPECIALGROUPCOLLAPSE}
  EBP_SPECIALGROUPEXPAND = 11;
  {$EXTERNALSYM EBP_SPECIALGROUPEXPAND}
  EBP_SPECIALGROUPHEAD = 12;
  {$EXTERNALSYM EBP_SPECIALGROUPHEAD}

  EBHC_NORMAL = 1;
  {$EXTERNALSYM EBHC_NORMAL}
  EBHC_HOT = 2;
  {$EXTERNALSYM EBHC_HOT}
  EBHC_PRESSED = 3;
  {$EXTERNALSYM EBHC_PRESSED}

  EBHP_NORMAL = 1;
  {$EXTERNALSYM EBHP_NORMAL}
  EBHP_HOT = 2;
  {$EXTERNALSYM EBHP_HOT}
  EBHP_PRESSED = 3;
  {$EXTERNALSYM EBHP_PRESSED}
  EBHP_SELECTEDNORMAL = 4;
  {$EXTERNALSYM EBHP_SELECTEDNORMAL}
  EBHP_SELECTEDHOT = 5;
  {$EXTERNALSYM EBHP_SELECTEDHOT}
  EBHP_SELECTEDPRESSED = 6;
  {$EXTERNALSYM EBHP_SELECTEDPRESSED}

  EBM_NORMAL = 1;
  {$EXTERNALSYM EBM_NORMAL}
  EBM_HOT = 2;
  {$EXTERNALSYM EBM_HOT}
  EBM_PRESSED = 3;
  {$EXTERNALSYM EBM_PRESSED}

  EBNGC_NORMAL = 1;
  {$EXTERNALSYM EBNGC_NORMAL}
  EBNGC_HOT = 2;
  {$EXTERNALSYM EBNGC_HOT}
  EBNGC_PRESSED = 3;
  {$EXTERNALSYM EBNGC_PRESSED}

  EBNGE_NORMAL = 1;
  {$EXTERNALSYM EBNGE_NORMAL}
  EBNGE_HOT = 2;
  {$EXTERNALSYM EBNGE_HOT}
  EBNGE_PRESSED = 3;
  {$EXTERNALSYM EBNGE_PRESSED}

  EBSGC_NORMAL = 1;
  {$EXTERNALSYM EBSGC_NORMAL}
  EBSGC_HOT = 2;
  {$EXTERNALSYM EBSGC_HOT}
  EBSGC_PRESSED = 3;
  {$EXTERNALSYM EBSGC_PRESSED}

  EBSGE_NORMAL = 1;
  {$EXTERNALSYM EBSGE_NORMAL}
  EBSGE_HOT = 2;
  {$EXTERNALSYM EBSGE_HOT}
  EBSGE_PRESSED = 3;
  {$EXTERNALSYM EBSGE_PRESSED}

//---------------------------------------------------------------------------------------
//   "TaskBand" Parts & States
//---------------------------------------------------------------------------------------
  MDP_NEWAPPBUTTON = 1;
  {$EXTERNALSYM MDP_NEWAPPBUTTON}
  MDP_SEPERATOR = 2;
  {$EXTERNALSYM MDP_SEPERATOR}

  MDS_NORMAL = 1;
  {$EXTERNALSYM MDS_NORMAL}
  MDS_HOT = 2;
  {$EXTERNALSYM MDS_HOT}
  MDS_PRESSED = 3;
  {$EXTERNALSYM MDS_PRESSED}
  MDS_DISABLED = 4;
  {$EXTERNALSYM MDS_DISABLED}
  MDS_CHECKED = 5;
  {$EXTERNALSYM MDS_CHECKED}
  MDS_HOTCHECKED = 6;
  {$EXTERNALSYM MDS_HOTCHECKED}

  DTT_GRAYED = $1; {// draw a grayed-out string}
  {$EXTERNALSYM DTT_GRAYED}

  HTTB_BACKGROUNDSEG = $0000;
  {$EXTERNALSYM HTTB_BACKGROUNDSEG}
  HTTB_FIXEDBORDER = $0002; {// Return code may be either HTCLIENT or HTBORDER.}
  {$EXTERNALSYM HTTB_FIXEDBORDER}
  HTTB_CAPTION = $0004;
  {$EXTERNALSYM HTTB_CAPTION}
  HTTB_RESIZINGBORDER_LEFT = $0010; {// Hit test left resizing border,}
  {$EXTERNALSYM HTTB_RESIZINGBORDER_LEFT}
  HTTB_RESIZINGBORDER_TOP = $0020; {// Hit test top resizing border}
  {$EXTERNALSYM HTTB_RESIZINGBORDER_TOP}
  HTTB_RESIZINGBORDER_RIGHT = $0040; {// Hit test right resizing border}
  {$EXTERNALSYM HTTB_RESIZINGBORDER_RIGHT}
  HTTB_RESIZINGBORDER_BOTTOM = $0080; {// Hit test bottom resizing border}
  {$EXTERNALSYM HTTB_RESIZINGBORDER_BOTTOM}
  HTTB_RESIZINGBORDER = (HTTB_RESIZINGBORDER_LEFT OR HTTB_RESIZINGBORDER_TOP);
  {$EXTERNALSYM HTTB_RESIZINGBORDER}
  HTTB_SIZINGTEMPLATE = $0100;
  {$EXTERNALSYM HTTB_SIZINGTEMPLATE}
  HTTB_SYSTEMSIZINGMARGINS = $0200;
  {$EXTERNALSYM HTTB_SYSTEMSIZINGMARGINS}

  MAX_INTLIST_COUNT = 10;
  {$EXTERNALSYM MAX_INTLIST_COUNT}

  ETDT_DISABLE = $00000001;
  {$EXTERNALSYM ETDT_DISABLE}
  ETDT_ENABLE = $00000002;
  {$EXTERNALSYM ETDT_ENABLE}
  ETDT_USETABTEXTURE = $00000004;
  {$EXTERNALSYM ETDT_USETABTEXTURE}
  ETDT_ENABLETAB = (ETDT_ENABLE OR ETDT_USETABTEXTURE);
  {$EXTERNALSYM ETDT_ENABLETAB}

  STAP_ALLOW_NONCLIENT = 1;
  {$EXTERNALSYM STAP_ALLOW_NONCLIENT}

  STAP_ALLOW_CONTROLS = 2;
  {$EXTERNALSYM STAP_ALLOW_CONTROLS}

  STAP_ALLOW_WEBCONTENT = 4;
  {$EXTERNALSYM STAP_ALLOW_WEBCONTENT}

  SZ_THDOCPROP_DISPLAYNAME = 'DisplayName';
  {$EXTERNALSYM SZ_THDOCPROP_DISPLAYNAME}

  SZ_THDOCPROP_CANONICALNAME = 'ThemeName';
  {$EXTERNALSYM SZ_THDOCPROP_CANONICALNAME}

  SZ_THDOCPROP_TOOLTIP = 'ToolTip';
  {$EXTERNALSYM SZ_THDOCPROP_TOOLTIP}

  SZ_THDOCPROP_AUTHOR = 'author';
  {$EXTERNALSYM SZ_THDOCPROP_AUTHOR}

  GP_BORDER = 1;
  {$EXTERNALSYM GP_BORDER}
  GP_LINEHORZ = 2;
  {$EXTERNALSYM GP_LINEHORZ}
  GP_LINEVERT = 3;
  {$EXTERNALSYM GP_LINEVERT}

  BSS_FLAT = 1;
  {$EXTERNALSYM BSS_FLAT}
  BSS_RAISED = 2;
  {$EXTERNALSYM BSS_RAISED}
  BSS_SUNKED = 4;
  {$EXTERNALSYM BSS_SUNKED}


type
  HTHEME = THandle;
  {$EXTERNALSYM HTHEME}

  _MARGINS = record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end {_MARGINS};
  {$EXTERNALSYM _MARGINS}
  MARGINS = _MARGINS;
  {$EXTERNALSYM MARGINS}
  PMARGINS = ^_MARGINS;
  {$EXTERNALSYM PMARGINS}

  _INTLIST = record
    iValueCount: Integer;
    iValues: Array[0..MAX_INTLIST_COUNT-1] of Integer;
  end {_INTLIST};
  {$EXTERNALSYM _INTLIST}
  INTLIST = _INTLIST;
  {$EXTERNALSYM INTLIST}
  PINTLIST = ^_INTLIST;
  {$EXTERNALSYM PINTLIST}

  TDTBGOPTS  = record
    dwSize: DWORD;
    dwFlags: DWORD;
    rcClip: TRect;
  end;
  {$EXTERNALSYM TDTBGOPTS}

  PDTBGOPTS = ^TDTBGOPTS;
  {$EXTERNALSYM PDTBGOPTS}

var
  OpenThemeData: function(hwnd: THandle; pszClassList: PWideChar): HTheme cdecl stdcall;
  {$EXTERNALSYM OpenThemeData}


  CloseThemeData: function(hTheme: HTHEME): THandle cdecl stdcall;
  {$EXTERNALSYM CloseThemeData}

  DrawThemeBackground: function(hTheme: HTHEME;
                                hdc: HDC;
                                iPartId: Integer;
                                iStateId: Integer;
                                const pRect: PRECT;
                                const pClipRect: PRECT): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeBackground}

  DrawThemeBackgroundEx: function(hTheme: HTHEME;
                                hdc: HDC;
                                iPartId: Integer;
                                iStateId: Integer;
                                const pRect: PRECT;
                                const pOptions: PDTBGOPTS): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeBackgroundEx}

  DrawThemeText: function(hTheme: HTHEME;
                          hdc: HDC;
                          iPartId: Integer;
                          iStateId: Integer;
                          var pszText: PWideChar;
                          iCharCount: Integer;
                          dwTextFlags: LongInt;
                          dwTextFlags2: LongInt;
                          const pRect: PRECT): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeText}

  GetThemeBackgroundContentRect: function(hTheme: HTHEME;
                                          hdc: HDC;
                                          iPartId: Integer;
                                          iStateId: Integer;
                                          const pBoundingRect: PRECT;
                                          var pContentRect: TRECT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeBackgroundContentRect}

  GetThemeBackgroundExtent: function(hTheme: HTHEME;
                                     hdc: HDC;
                                     iPartId: Integer;
                                     iStateId: Integer;
                                     const pContentRect: PRECT;
                                     var pExtentRect: TRECT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeBackgroundExtent}

  GetThemeTextExtent: function(hTheme: HTHEME;
                               hdc: HDC;
                               iPartId: Integer;
                               iStateId: Integer;
                               var pszText: PWideChar;
                               iCharCount: Integer;
                               dwTextFlags: LongInt;
                               const pBoundingRect: PRECT;
                               var pExtentRect: TRECT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeTextExtent}

  GetThemeTextMetrics: function(hTheme: HTHEME;
                                hdc: HDC;
                                iPartId: Integer;
                                iStateId: Integer;
                                var ptm: TTEXTMETRIC): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeTextMetrics}

  GetThemeBackgroundRegion: function(hTheme: HTHEME;
                                     hdc: HDC;
                                     iPartId: Integer;
                                     iStateId: Integer;
                                     const pRect: PRECT;
                                     var pRegion: HRGN): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeBackgroundRegion}

  HitTestThemeBackground: function(hTheme: HTHEME;
                                   hdc: HDC;
                                   iPartId: Integer;
                                   iStateId: Integer;
                                   dwOptions: LongInt;
                                   const pRect: PRECT;
                                   hrgn: HRGN;
                                   var ptTest: Integer;
                                   var pwHitTestCode: WORD): THandle cdecl stdcall;
  {$EXTERNALSYM HitTestThemeBackground}

  DrawThemeEdge: function(hTheme: HTHEME;
                          hdc: HDC;
                          iPartId: Integer;
                          iStateId: Integer;
                          const pDestRect: PRECT;
                          uEdge: Word;
                          uFlags: Word;
                          var pContentRect: TRECT): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeEdge}

  DrawThemeIcon: function(hTheme: HTHEME;
                          hdc: HDC;
                          iPartId: Integer;
                          iStateId: Integer;
                          const pRect: PRECT;
                          himl: THandle;
                          iImageIndex: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeIcon}

  IsThemePartDefined: function(hTheme: HTHEME;
                      iPartId: Integer;
                      iStateId: Integer): Integer cdecl stdcall;
  {$EXTERNALSYM IsThemePartDefined}

  IsThemeBackGroundPartiallyTransparent: function(hTheme: HTHEME;
                      iPartId: Integer;
                      iStateId: Integer): Integer cdecl stdcall;
  {$EXTERNALSYM IsThemeBackGroundPartiallyTransparent}

  GetThemeColor: function(hTheme: HTHEME;
                          iPartId: Integer;
                          iStateId: Integer;
                          iPropId: Integer;
                          var pColor: TCOLOR): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeColor}

  GetThemeMetric: function(hTheme: HTHEME;
                           hdc: HDC;
                           iPartId: Integer;
                           iStateId: Integer;
                           iPropId: Integer;
                           var piVal: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeMetric}

  GetThemeString: function(hTheme: HTHEME;
                           iPartId: Integer;
                           iStateId: Integer;
                           iPropId: Integer;
                           pszBuff: PWideChar;
                           cchMaxBuffChars: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeString}

  GetThemeBool: function(hTheme: HTHEME;
                         iPartId: Integer;
                         iStateId: Integer;
                         iPropId: Integer;
                         var pfVal: Bool): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeBool}

  GetThemeInt: function(hTheme: HTHEME;
                        iPartId: Integer;
                        iStateId: Integer;
                        iPropId: Integer;
                        var piVal: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeInt}

  GetThemeEnumValue: function(hTheme: HTHEME;
                              iPartId: Integer;
                              iStateId: Integer;
                              iPropId: Integer;
                              var piVal: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeEnumValue}

  GetThemePosition: function(hTheme: HTHEME;
                             iPartId: Integer;
                             iStateId: Integer;
                             iPropId: Integer;
                             var pPoint: TPOINT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemePosition}

  GetThemeFont: function(hTheme: HTHEME;
                         hdc: HDC;
                         iPartId: Integer;
                         iStateId: Integer;
                         iPropId: Integer;
                         var pFont: TLOGFONT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeFont}

  GetThemeRect: function(hTheme: HTHEME;
                         iPartId: Integer;
                         iStateId: Integer;
                         iPropId: Integer;
                         var pRect: TRECT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeRect}

  GetThemeMargins: function(hTheme: HTHEME;
                            hdc: HDC;
                            iPartId: Integer;
                            iStateId: Integer;
                            iPropId: Integer;
                            var prc: TRECT;
                            var pMargins: TRect): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeMargins}

  GetThemeIntList: function(hTheme: HTHEME;
                            iPartId: Integer;
                            iStateId: Integer;
                            iPropId: Integer;
                            var pIntList: Pointer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeIntList}

  SetWindowTheme: function(hwnd: HWND;
                           var pszSubAppName: PWideChar;
                           var pszSubIdList: PWideChar): THandle cdecl stdcall;
  {$EXTERNALSYM SetWindowTheme}

  GetThemeFilename: function(hTheme: HTHEME;
                             iPartId: Integer;
                             iStateId: Integer;
                             iPropId: Integer;
                             pszThemeFileName: PWideChar;
                             cchMaxBuffChars: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeFilename}

  GetThemeSysColor: function(hTheme: HTHEME;
                      iColorId: Integer): Integer cdecl stdcall;
  {$EXTERNALSYM GetThemeSysColor}

  GetThemeSysColorBrush: function(hTheme: HTHEME;
                      iColorId: Integer): Integer cdecl stdcall;
  {$EXTERNALSYM GetThemeSysColorBrush}

  GetThemeSysBool: function(hTheme: HTHEME;
                      iBoolId: Integer): Integer cdecl stdcall;
  {$EXTERNALSYM GetThemeSysBool}

  GetThemeSysSize: function(hTheme: HTHEME;
                      iSizeId: Integer): Integer cdecl stdcall;
  {$EXTERNALSYM GetThemeSysSize}

  GetThemeSysFont: function(hTheme: HTHEME;
                            iFontId: Integer;
                            var plf: TLOGFONT): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeSysFont}

  GetThemeSysString: function(hTheme: HTHEME;
                              iStringId: Integer;
                              pszStringBuff: PWideChar;
                              cchMaxStringChars: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeSysString}

  GetThemeSysInt: function(hTheme: HTHEME;
                           iIntId: Integer;
                           var piValue: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeSysInt}

  IsThemeActive: function: BOOL cdecl stdcall;
  {$EXTERNALSYM IsThemeActive}

  IsAppThemed: function: BOOL cdecl stdcall;
  {$EXTERNALSYM IsAppThemed}

  GetWindowTheme: function(hwnd: HWnd): Integer cdecl stdcall;
  {$EXTERNALSYM GetWindowTheme}

  EnableThemeDialogTexture: function(hwnd: HWND;
                                     dwFlags: LongInt): THandle cdecl stdcall;
  {$EXTERNALSYM EnableThemeDialogTexture}

  IsThemeDialogTextureEnabled: function(hwnd: HWnd): BOOL cdecl stdcall;
  {$EXTERNALSYM IsThemeDialogTextureEnabled}

  GetThemeAppProperties: function: DWORD cdecl stdcall;
  {$EXTERNALSYM GetThemeAppProperties}

  SetThemeAppProperties: function(dwFlags: DWORD): Integer cdecl stdcall;
  {$EXTERNALSYM SetThemeAppProperties}

  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
                                cchMaxNameChars: Integer;
                                pszColorBuff: PWideChar;
                                cchMaxColorChars: Integer;
                                pszSizeBuff: PWideChar;
                                cchMaxSizeChars: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetCurrentThemeName}

  GetThemeDocumentationProperty: function(pszThemeName: PWideChar;
                                          var pszPropertyName: PWideChar;
                                          pszValueBuff: PWideChar;
                                          cchMaxValChars: Integer): THandle cdecl stdcall;
  {$EXTERNALSYM GetThemeDocumentationProperty}

  DrawThemeParentBackground: function(hwnd: HWND;
                                      hdc: HDC;
                                      var prc: TRECT): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeParentBackground}

  EnableTheming: function(fEnable: Bool): THandle cdecl stdcall;
  {$EXTERNALSYM EnableTheming}

implementation

var
  DLLLoaded: Boolean = False;
  DLLHandle: THandle;

procedure UnLoadDLL;
begin
  if DLLLoaded then
  begin
    FreeLibrary(DLLHandle);
    DLLLoaded := false;
  end;
end;

procedure LoadDLL;
begin
  if DLLLoaded then Exit;
  
  DLLHandle := LoadLibrary('UXTHEME.DLL');
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
    
    @OpenThemeData := GetProcAddress(DLLHandle,'OpenThemeData');
    Assert(@OpenThemeData <> nil);

    @CloseThemeData := GetProcAddress(DLLHandle,'CloseThemeData');
    Assert(@CloseThemeData <> nil);

    @DrawThemeBackground := GetProcAddress(DLLHandle,'DrawThemeBackground');
    Assert(@DrawThemeBackground <> nil);

    @DrawThemeText := GetProcAddress(DLLHandle,'DrawThemeText');
    Assert(@DrawThemeText <> nil);

    @GetThemeBackgroundContentRect := GetProcAddress(DLLHandle,'GetThemeBackgroundContentRect');
    Assert(@GetThemeBackgroundContentRect <> nil);

    @GetThemeBackgroundExtent := GetProcAddress(DLLHandle,'GetThemeBackgroundExtent');
    Assert(@GetThemeBackgroundExtent <> nil);

    @GetThemeTextExtent := GetProcAddress(DLLHandle,'GetThemeTextExtent');
    Assert(@GetThemeTextExtent <> nil);

    @GetThemeTextMetrics := GetProcAddress(DLLHandle,'GetThemeTextMetrics');
    Assert(@GetThemeTextMetrics <> nil);

    @GetThemeBackgroundRegion := GetProcAddress(DLLHandle,'GetThemeBackgroundRegion');
    Assert(@GetThemeBackgroundRegion <> nil);

    @HitTestThemeBackground := GetProcAddress(DLLHandle,'HitTestThemeBackground');
    Assert(@HitTestThemeBackground <> nil);

    @DrawThemeEdge := GetProcAddress(DLLHandle,'DrawThemeEdge');
    Assert(@DrawThemeEdge <> nil);

    @DrawThemeIcon := GetProcAddress(DLLHandle,'DrawThemeIcon');
    Assert(@DrawThemeIcon <> nil);

    @IsThemePartDefined := GetProcAddress(DLLHandle,'IsThemePartDefined');
    Assert(@IsThemePartDefined <> nil);

    @IsThemeBackGroundPartiallyTransparent := GetProcAddress(DLLHandle,'IsThemeBackgroundPartiallyTransparent');
    Assert(@IsThemeBackGroundPartiallyTransparent <> nil);

    @GetThemeColor := GetProcAddress(DLLHandle,'GetThemeColor');
    Assert(@GetThemeColor <> nil);

    @GetThemeMetric := GetProcAddress(DLLHandle,'GetThemeMetric');
    Assert(@GetThemeMetric <> nil);

    @GetThemeString := GetProcAddress(DLLHandle,'GetThemeString');
    Assert(@GetThemeString <> nil);

    @GetThemeBool := GetProcAddress(DLLHandle,'GetThemeBool');
    Assert(@GetThemeBool <> nil);

    @GetThemeInt := GetProcAddress(DLLHandle,'GetThemeInt');
    Assert(@GetThemeInt <> nil);

    @GetThemeEnumValue := GetProcAddress(DLLHandle,'GetThemeEnumValue');
    Assert(@GetThemeEnumValue <> nil);

    @GetThemePosition := GetProcAddress(DLLHandle,'GetThemePosition');
    Assert(@GetThemePosition <> nil);

    @GetThemeFont := GetProcAddress(DLLHandle,'GetThemeFont');
    Assert(@GetThemeFont <> nil);

    @GetThemeRect := GetProcAddress(DLLHandle,'GetThemeRect');
    Assert(@GetThemeRect <> nil);

    @GetThemeMargins := GetProcAddress(DLLHandle,'GetThemeMargins');
    Assert(@GetThemeMargins <> nil);

    @GetThemeIntList := GetProcAddress(DLLHandle,'GetThemeIntList');
    Assert(@GetThemeIntList <> nil);

    @SetWindowTheme := GetProcAddress(DLLHandle,'SetWindowTheme');
    Assert(@SetWindowTheme <> nil);

    @GetThemeFilename := GetProcAddress(DLLHandle,'GetThemeFilename');
    Assert(@GetThemeFilename <> nil);

    @GetThemeSysColor := GetProcAddress(DLLHandle,'GetThemeSysColor');
    Assert(@GetThemeSysColor <> nil);

    @GetThemeSysColorBrush := GetProcAddress(DLLHandle,'GetThemeSysColorBrush');
    Assert(@GetThemeSysColorBrush <> nil);

    @GetThemeSysBool := GetProcAddress(DLLHandle,'GetThemeSysBool');
    Assert(@GetThemeSysBool <> nil);

    @GetThemeSysSize := GetProcAddress(DLLHandle,'GetThemeSysSize');
    Assert(@GetThemeSysSize <> nil);

    @GetThemeSysFont := GetProcAddress(DLLHandle,'GetThemeSysFont');
    Assert(@GetThemeSysFont <> nil);

    @GetThemeSysString := GetProcAddress(DLLHandle,'GetThemeSysString');
    Assert(@GetThemeSysString <> nil);

    @GetThemeSysInt := GetProcAddress(DLLHandle,'GetThemeSysInt');
    Assert(@GetThemeSysInt <> nil);

    @IsThemeActive := GetProcAddress(DLLHandle,'IsThemeActive');
    Assert(@IsThemeActive <> nil);

    @IsAppThemed := GetProcAddress(DLLHandle,'IsAppThemed');
    Assert(@IsAppThemed <> nil);

    @GetWindowTheme := GetProcAddress(DLLHandle,'GetWindowTheme');
    Assert(@GetWindowTheme <> nil);

    @EnableThemeDialogTexture := GetProcAddress(DLLHandle,'EnableThemeDialogTexture');
    Assert(@EnableThemeDialogTexture <> nil);

    @IsThemeDialogTextureEnabled := GetProcAddress(DLLHandle,'IsThemeDialogTextureEnabled');
    Assert(@IsThemeDialogTextureEnabled <> nil);

    @GetThemeAppProperties := GetProcAddress(DLLHandle,'GetThemeAppProperties');
    Assert(@GetThemeAppProperties <> nil);

    @SetThemeAppProperties := GetProcAddress(DLLHandle,'SetThemeAppProperties');
    Assert(@SetThemeAppProperties <> nil);

    @GetCurrentThemeName := GetProcAddress(DLLHandle,'GetCurrentThemeName');
    Assert(@GetCurrentThemeName <> nil);

    @GetThemeDocumentationProperty := GetProcAddress(DLLHandle,'GetThemeDocumentationProperty');
    Assert(@GetThemeDocumentationProperty <> nil);

    @DrawThemeParentBackground := GetProcAddress(DLLHandle,'DrawThemeParentBackground');
    Assert(@DrawThemeParentBackground <> nil);

    @EnableTheming := GetProcAddress(DLLHandle,'EnableTheming');
    Assert(@EnableTheming <> nil);
  end
  else
  begin
    DLLLoaded := False;
    { Error: UXTHEME.DLL could not be loaded !! }
  end;

end;

initialization
  LoadDLL;
  
finalization
  UnLoadDLL;

end.
