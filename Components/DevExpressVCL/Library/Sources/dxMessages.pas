{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxMessages;

{$I cxVer.inc}

interface

uses
  Messages;

const
  WM_DX = WM_APP + 100;

// cxControls
  DXM_NCSIZECHANGED = WM_DX + 1;
  DXM_SYNCHRONIZETHREADS = WM_DX + 2;
  DXM_CONTAINERSETFOCUS = WM_DX + 3;
  DXM_REFRESHCUSTOMIZATION = WM_DX + 4;
  DXM_UPDATEIMAGES = WM_DX + 5;
  DXM_RECALCULATE = WM_DX + 6;
  DXM_POSTAPPKEYDOWN = WM_DX + 7;
  DXM_SCALECHANGING = WM_DX + 8;
  DXM_SCALECHANGED = WM_DX + 9;
  DXM_UIADORNERMANAGERUPDATE = WM_DX + 10;

// cxContainer
  DXM_CLOSEPOPUPWINDOW = WM_DX + 25;
  DXM_SHOWPOPUPWINDOW = WM_DX + 26;
  DXM_SHORTREFRESHCONTAINER = WM_DX + 27;
  DXM_UPDATESCROLLBARS = WM_DX + 28;
  DXM_BUFFEREDPAINTONGLASS = WM_DX + 29;
  DXM_UPDATEEDITVALUE = WM_DX + 30;
  DXM_UPDATEWINDOWREGION = WM_DX + 31;

// cxDropDownEdit
  DXM_POPUPCONTROLKEY = WM_DX + 50;
  DXM_DROPDOWNBYPASTE = WM_DX + 51;

// cxButton
  DXM_DROPDOWNPOPUPMENU = WM_DX + 52;
  DXM_CLOSEUPPOPUPMENU = WM_DX + 53;

// cxGroupBox
  DXM_UPDATENONCLIENTAREA = WM_DX + 54;

// cxHeader
  DXM_GETHEADERITEMINFO = WM_DX + 55;

// cxSplitter
  DXM_ADJUSTPOSITION = WM_DX + 56;


// cxVG
  DXM_VG_PROPERTYCHANGED = WM_DX + 75;

// dxBar
  DXM_BAR_REPAINTBAR = WM_DX + 100;
  DXM_BAR_LB_SYNCHRONIZE = WM_DX + 101;
  DXM_BAR_LB_SYNCHRONIZESELECTION = WM_DX + 102;
  DXM_BAR_LB_UPDATEEVENTS = WM_DX + 103;
  DXM_BAR_LB_DEFERREDCALLSYNCHRONIZATION = WM_DX + 104;
  DXM_BAR_SELECTAPPMENUFIRSTITEMCONTROL = WM_DX + 105;
  DXM_BAR_SHOWKEYTIPS = WM_DX + 106;
  DXM_BAR_FASTCOMMAND = WM_DX + 107;
  DXM_BAR_SHOWSYSTEMMENU = WM_DX + 108;
  DXM_BAR_HIDEALL = WM_DX + 110;
  DXM_BAR_MERGE = WM_DX + 111;

// dxDocking
  DXM_DOCK_DESTROYCONTROLS = WM_DX + 125;
  DXM_DOCK_PURGEPARENT = WM_DX + 126;
  DXM_DOCK_CHECKACTIVEDOCKCONTROL = WM_DX + 127;

// dxLayout
  DXM_LAYOUT_PLACECONTROLS = WM_DX + 151;
  DXM_LAYOUT_BUILDSELECTIONLAYER = WM_DX + 152;
  DXM_LAYOUT_INVALIDATESELECTIONLAYER = WM_DX + 153;
  DXM_LAYOUT_LAYOUTCHANGED = WM_DX + 154;
  DXM_LAYOUT_UPDATEFLOAT = WM_DX + 155;
  DXM_LAYOUT_UPDATESCROLLPOS = WM_DX + 156;

// dxSkins
  DXM_SKINS_POSTREDRAW = WM_DX + 175;
  DXM_SKINS_CHILDCHANGED = WM_DX + 176;
  DXM_SKINS_POSTCHECKRGN = WM_DX + 177;
  DXM_SKINS_POSTCREATE = WM_DX + 178;
  DXM_SKINS_POSTMSGFORMINIT = WM_DX + 179;
  DXM_SKINS_HASOWNSKINENGINE = WM_DX + 180;
  DXM_SKINS_SUPPRESSMDICHILDBORDERS = WM_DX + 181;
  DXM_SKINS_GETISSKINNED = WM_DX + 182;
  DXM_SKINS_SETISSKINNED = WM_DX + 183;

// Spell
  DXM_SPELL_AUTOCORRECT = WM_DX + 200;
  DXM_SPELL_REDRAWMISSPELLINGS = WM_DX + 201;

// PS
  DXM_PS_FREEEXPLORERITEM = WM_DX + 225;
  DXM_PS_UPDATEMARGINS = WM_DX + 226;
  DXM_PS_UPDATESTATUSPROGRESS = WM_DX + 227;
  DXM_PS_PRINTSTYLELISTCHANGED = WM_DX + 228;
  DXM_PS_PRINTERLISTCHANGED = WM_DX + 229;
  DXM_PS_INITIALIZEPRINTER = WM_DX + 230;
  DXM_PS_CREATEFOLDER = WM_DX + 231;

//dxBreadCrumbEdit
  DXM_BREADCRUMBEDIT_CLOSEPOPUPWINDOW = WM_DX + 250;
  DXM_BREADCRUMBEDIT_HIDESUGGESTIONS = WM_DX + 251;
  DXM_BREADCRUMBEDIT_RELEASEPATHEDITOR = WM_DX + 252;

// dxRibbon
  DXM_RIBBONFORM_NCCHANGED = WM_DX + 275;
  DXM_RIBBONFORM_SYSCOMMAND = WM_DX + 276;
  DXM_RIBBONFORM_POSTUPDATEREGION = WM_DX + 277;
  DXM_RIBBON_SHOWAPPLICATIONMENU = WM_DX + 278;
  DXM_RIBBON_RECALCULATE = WM_DX + 279;

//cxFilterControl
  DXWM_FILTERCONTROL_DROPDOWNMENUCLOSED = WM_DX + 300;

//dxTileControl
  DXM_TILECONTROL_HIDEGROUPCAPTIONEDITORS = WM_DX + 325;

  DXM_UITHREADINVOKE = WM_DX + 350;
//RichEditControl
  DXM_RICHEDITCONTROL_UITHREADINVOKE = DXM_UITHREADINVOKE;

// dxSpreadSheet
  DXM_CANCELHINT = WM_DX + 370;
  DXM_POSTHIDEEDIT = WM_DX + 371;

implementation

end.
