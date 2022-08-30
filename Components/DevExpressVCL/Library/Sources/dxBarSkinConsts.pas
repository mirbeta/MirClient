﻿{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarSkinConsts;

{$I cxVer.inc}

interface

const
//Parts
  DXBAR_ITEMSEPARATOR = 0;
  DXBAR_ITEMSEPARATOR_VERT = 2;
  DXBAR_ITEMTEXT = 335;
  DXBAR_TOOLBAR = 1;
  DXBAR_MARKARROW = 336;
  DXBAR_MARKTRUNCATED = 337;
  DXBAR_MARKARROWINPOPUP = 338;
  DXBAR_ARROWDOWN = 340;
  DXBAR_QUICKACCESSARROWDOWN = 341;
  DXBAR_SCROLLARROW = 360;
//  DXBAR_MENU = 360;
  DXBAR_MENUITEM = 361;
  DXBAR_MENUITEMTEXT = 362;
  DXBAR_MENUITEM_GLYPH = 363;
  DXBAR_MENUITEM_DROPBUTTON = 364;
  DXBAR_MENUARROWDOWN = 365;
  DXBAR_MENUARROWRIGHT = 366;
  DXBAR_MENUBUTTONITEMTEXT = 367;
  DXBAR_MENUBACKBUTTON = 368;
  DXBAR_MENUDETACHCAPTION = 370;
  DXBAR_MENUDETACHCAPTIONAREA = 375;
  DXBAR_MENUGLYPH = 380;
  DXBAR_MENUCONTENT = 390;

  // ExtraPane
  DXBAR_MENUEXTRAPANE = 391;
  DXBAR_MENUEXTRAPANE_BUTTON = 392;
  DXBAR_MENUEXTRAPANE_BUTTON_TEXTCOLOR = 393;
  DXBAR_MENUEXTRAPANE_HEADER_TEXTCOLOR = 394;
  DXBAR_MENUEXTRAPANE_PINBUTTON = 395;
  DXBAR_MENUEXTRAPANE_SEPARATOR = 396;
  DXBAR_MENUEXTRAPANE_SEPARATOR_VERT = 397;

  DXBAR_MENUSEPARATORHORZ = 400;
  DXBAR_MENUSEPARATORVERT = 410;
  DXBAR_MENUARROWSEPARATOR = 420;
  DXBAR_MENUEDITSEPARATOR = 425;
  DXBAR_MENUCHECK = 430;
  DXBAR_MENUCHECKMARK = 440;
  DXBAR_MENUMARK = 442;
  DXBAR_MENUSCROLLAREA = 446;
  DXBAR_DROPDOWNBORDER = 450;
  DXBAR_DROPDOWNBORDER_INNERLINE = 451;
  DXBAR_COLLAPSEDTOOLBAR = 1000;
  DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND = 1001;
  DXBAR_QUICKACCESSTOOLBAR = 1002;
  DXBAR_TOOLBARINPOPUP = 1003;

  DXBAR_RIBBONTABGROUP = 1010;
  DXBAR_RIBBONCONTEXTTABGROUP = 1011;
  DXBAR_RIBBONGROUPGAP = 1012;
  DXBAR_TABSGROUPSOVERLAPHEIGHT = 1013;

  //ApplicationMenu
  DXBAR_APPLICATIONMENU = 452;
  DXBAR_APPLICATIONMENUCONTENT = 453;
  DXBAR_APPLICATIONMENUBUTTON = 454;
  DXBAR_APPLICATIONBUTTON = 455;
  DXBAR_APPLICATIONBUTTONICONOFFSET = 459;

  //Button
  DXBAR_SMALLBUTTON = 3;
  DXBAR_SMALLBUTTON_DROPBUTTON = 4;
  DXBAR_SMALLBUTTON_GLYPH = 5;
  DXBAR_LARGEBUTTON = 6;
  DXBAR_LARGEBUTTON_DROPBUTTON = 7;
  DXBAR_LARGEBUTTON_GLYPH = 8;
  DXBAR_BUTTONGROUP = 460;
  DXBAR_BUTTONGROUP_DROPBUTTON = 461;
  DXBAR_BUTTONGROUP_GLYPH = 462;
  DXBAR_BUTTONITEMTEXT = 463;
  DXBAR_BUTTONGROUPBORDERLEFT = 470;
  DXBAR_BUTTONGROUPBORDERMIDDLE = 480;
  DXBAR_BUTTONGROUPBORDERRIGHT = 490;
  DXBAR_BUTTONGROUPSPLITBUTTONSEPARATOR = 500;
  DXBAR_LAUNCHBUTTONDEFAULTGLYPH = 510;
  DXBAR_LAUNCHBUTTONBACKGROUND = 515;

  //Edit
  DXBAR_EDIT_BACKGROUND = 9;
  DXBAR_EDIT_BORDER = 10;
  DXBAR_EDIT_BUTTON = 11;
  DXBAR_EDIT_BUTTON_BORDER = 12;
  DXBAR_EDIT_ARROWBUTTON = 13;
  DXBAR_EDIT_ELLIPSISBUTTON = 14;
  DXBAR_EDIT_TEXTCOLOR = 17;
  DXBAR_SPINEDIT_DOWNBUTTON = 15;
  DXBAR_SPINEDIT_UPBUTTON = 16;

  //Date Navigator
  DXBAR_DATENAVIGATOR_HEADER = 19;

  //Separator
  DXBAR_SEPARATOR_BACKGROUND = 20;
  DXBAR_SEPARATOR_LINE = 21;
  DXBAR_SEPARATOR_TEXTCOLOR = 22;

  //Progress
  DXBAR_PROGRESSSOLIDBAND = 30;
  DXBAR_PROGRESSDISCRETEBAND = 31;
  DXBAR_PROGRESSSUBSTRATE = 32;

  //ToolTips
  DXBAR_SCREENTIP = 40;
  DXBAR_SCREENTIP_FOOTERLINE = 41;
  DXBAR_SCREENTIP_TITLE = 42;
  DXBAR_SCREENTIP_DESCRIPTION = 43;
  DXBAR_KEYTIP_TEXTCOLOR = 44;

  //Gallery
  DXBAR_INRIBBONGALLERY = 50;
  DXBAR_INRIBBONGALLERY_BACKGROUND = 51;
  DXBAR_INRIBBONGALLERY_BORDER = 52;
  DXBAR_INRIBBONGALLERYSCROLLBAR_LINEUPBUTTON = 53;
  DXBAR_INRIBBONGALLERYSCROLLBAR_LINEDOWNBUTTON = 54;
  DXBAR_INRIBBONGALLERYSCROLLBAR_DROPDOWNBUTTON = 55;
  DXBAR_DROPDOWNGALLERY = 56;
  DXBAR_DROPDOWNGALLERY_TOPSIZINGBAND = 57;
  DXBAR_DROPDOWNGALLERY_BOTTOMSIZINGBAND = 58;
  DXBAR_DROPDOWNGALLERY_TOPSIZEGRIP = 59;
  DXBAR_DROPDOWNGALLERY_BOTTOMSIZEGRIP = 60;
  DXBAR_DROPDOWNGALLERY_TOPVERTICALSIZEGRIP = 61;
  DXBAR_DROPDOWNGALLERY_BOTTOMVERTICALSIZEGRIP = 62;
  DXBAR_GALLERYGROUPHEADERBACKGROUND = 63;
  DXBAR_GALLERYGROUPHEADERTEXT = 64;
  DXBAR_GALLERYFILTERBAND = 65;
  DXBAR_GALLERYFILTERBANDTEXT = 66;
  DXBAR_GALLERYGROUPITEM_SELECTIONFRAME = 67;
  DXBAR_INRIBBONGALLERYSCROLLBAR_BACKGROUND = 69;
  DXBAR_INRIBBONGALLERYSCROLLBAR_DROPDOWNBUTTON_TOUCH = 70;
  DXBAR_INRIBBONGALLERYITEM_TEXTCOLOR = 200;
  DXBAR_DROPDOWNGALLERYITEM_TExTCOLOR = 201;

  //MiniToobar
  DXBAR_MINITOOLBAR = 70;
  DXBAR_MINITOOLBAR_BACKGROUND = 71;

  //RadialMenu
  DXBAR_RADIALMENUACCENT = 80;
  DXBAR_RADIALMENUBACKGROUND = 81;

  // ScrollBar
  DXBAR_SCROLLBARHORZ_BACKGROUND = 100;
  DXBAR_SCROLLBARHORZ_LINEUP = 101;
  DXBAR_SCROLLBARHORZ_LINEDOWN = 102;
  DXBAR_SCROLLBARHORZ_THUMBNAIL = 103;
  DXBAR_SCROLLBARHORZ_PAGEUP = 104;
  DXBAR_SCROLLBARHORZ_PAGEDOWN = 105;

  DXBAR_SCROLLBARVERT_BACKGROUND = 106;
  DXBAR_SCROLLBARVERT_LINEUP = 107;
  DXBAR_SCROLLBARVERT_LINEDOWN = 108;
  DXBAR_SCROLLBARVERT_THUMBNAIL = 109;
  DXBAR_SCROLLBARVERT_PAGEUP = 110;
  DXBAR_SCROLLBARVERT_PAGEDOWN = 111;

  DXBAR_SCROLLBOX_SIZEGRIPAREA = 112;

  // BackstageView
  DXBAR_BACKSTAGEVIEW = 250;
  DXBAR_BACKSTAGEVIEW_TEXTCOLOR = 251;
  DXBAR_BACKSTAGEVIEW_MENUBAR = 252;
  DXBAR_BACKSTAGEVIEW_MENUBAR_HEADER = 253;
  DXBAR_BACKSTAGEVIEW_MENUBAR_INDENTBETWEENITEMS = 254;
  DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM = 260;
  DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR = 261;
  DXBAR_BACKSTAGEVIEW_MENUBAR_TAB = 270;
  DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR = 271;
  DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR = 280;
  DXBAR_BACKSTAGEVIEW_BACKBUTTON = 281;
  DXBAR_BACKSTAGEVIEW_BACKBUTTON_OFFSET = 282;

  // BackstageViewGalleryControl
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL = 300;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_BORDER = 301;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR = 302;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER = 305;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR = 306;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM = 310;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTON = 311;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH = 312;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINTAG = 313;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR = 314;
  DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR = 315;

// States
  DXBAR_STATESCOUNT = 9;

  DXBAR_NORMAL = 0;
  DXBAR_DISABLED = 1;
  DXBAR_HOT = 2;
  DXBAR_ACTIVE = 3;
  DXBAR_PRESSED = 4;
  DXBAR_FOCUSED = 4;
  DXBAR_DROPPEDDOWN = 5;
  DXBAR_CHECKED = 6;
  DXBAR_HOTCHECK = 7;
  DXBAR_ACTIVEDISABLED = 8;

  DXBAR_BTN_STATES = [DXBAR_HOT..DXBAR_ACTIVEDISABLED];

implementation

end.
