{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit cxLibraryConsts;

{$I cxVer.inc}

interface

const
  SCircularReferencingError = 'Circular referencing is not allowed';

  // Button Repeat Interval
  cxButtonsRepeatInitPause = 400;
  cxButtonsRepeatPause = 100;

  // Cursors
    // cxControls
  crBase = 2100;
  crDragCopy = crBase;
  crcxRemove = crBase + 8;
  crcxVertSize = crBase + 9;
  crcxHorzSize = crBase + 10;
  crcxDragMulti = crBase + 11;
  crcxNoDrop = crBase + 12;
  crcxDrag = crBase + 13;
  crcxColorPicker = crBase + 14;
  crcxHandPoint = -crBase + 1;
  crcxMultiDragCopy = crBase + 15;
  crcxHand = crBase + 16;
  crcxHandDrag = crBase + 17;

  crdxLayoutControlDrag = crBase + 18;
  crdxLayoutControlNoDrop = crBase + 19;
  crdxLayoutControlRemove = crBase + 20;
  crdxLayoutControlDropAfter = crBase + 21;
  crdxLayoutControlDropBefore = crBase + 22;
  crdxLayoutControlDropInside = crBase + 23;
  crcxDropAfterCopy = crBase + 24;
  crcxDropBeforeCopy = crBase + 25;
  crcxDropInsideCopy = crBase + 26;

    // dxNavBar
  dxNavBarDragCursor = -1120;
  dxNavBarDragCopyCursor = -1119;
  dxNavBarLinksCursor = -1118;

    // dxBars
  crdxBarDrag = 1041;
  crdxBarDragCopy = 1042;
  crdxBarDragNoDrop = 1043;
  crdxBarEditSizing = 1044;

    // dxSideBar
  dxSideBarDragCursor = -1121;
  dxSideBarDragCopyCursor = -1122;
  dxSideBarDragDeleteCursor = -1123;
  dxSideBarGroupCursor = -1125;

    // dxLayout
  dxLayoutControlCursorBase = 3000; // moved to cxControls

    // cxEdit
  crcxEditBase       = 4101;
  crcxEditMouseWheel = crcxEditBase;

    // cxGrid
  crcxGridBase = 4000;
  crcxGridHorzSize: Integer = crcxHorzSize;
  crcxGridVertSize: Integer = crcxVertSize;
  crcxGridRemove: Integer = crcxRemove;
  crcxGridNoDrop: Integer = crcxNoDrop;
  crcxGridDrag: Integer = crcxDrag;
  crcxGridMultiDrag: Integer = crcxDragMulti;
  crcxGridSelectRow: Integer = crcxGridBase + 1;
  crcxGridMagnifier: Integer = crcxGridBase + 2;
  crcxGridChartDrag: Integer = crcxGridBase + 3;

    // cxScheduler
  crSchedulerCopyEvent  = 4201;
  crSchedulerMoveEvent  = 4202;
  crSchedulerHorzSplit  = 4203;
  crSchedulerVertSplit  = 4204;
  crSchedulerSplitAll   = 4205;
  crSchedulerVertResize = 4206;
  crSchedulerHorzResize = 4207;
  crCalendarMirrorArrow = 4208;
  crTaskLink            = 4209;

    // cxPivotGrid
  crcxPivotGridBase = 4300;
  crcxPivotGridArrow: Integer = crcxPivotGridBase + 1;
  crcxPivotGridHorzSize: Integer = crcxPivotGridBase + 2;
  crcxPivotGridNoDrop: Integer = crcxPivotGridBase + 3;
  crcxPivotGridRemove: Integer = crcxPivotGridBase + 4;

  // dxRichEditControl
  crcxRichEditControlBase = 4400;
  crcxRichEditControlBeginRotate = crcxRichEditControlBase + 1;
  crcxRichEditControlIBeamItalic = crcxRichEditControlBase + 2;
  crcxRichEditControlResizeColumn = crcxRichEditControlBase + 3;
  crcxRichEditControlResizeRow = crcxRichEditControlBase + 4;
  crcxRichEditControlReverseArrow = crcxRichEditControlBase + 5;
  crcxRichEditControlRotate = crcxRichEditControlBase + 6;
  crcxRichEditControlSelectColumn = crcxRichEditControlBase + 7;
  crcxRichEditControlSelectTableCell = crcxRichEditControlBase + 8;

  // cxSpreadSheet
  crdxSpreadSheetBase = 3200;
  crSSSelect = crdxSpreadSheetBase;
  crdxSpreadSheetRotate = crdxSpreadSheetBase + 1;
  crdxSpreadSheetRotation = crdxSpreadSheetBase + 2;
  crdxSpreadSheetDownArrow = crdxSpreadSheetBase + 3;
  crdxSpreadSheetLeftArrow = crdxSpreadSheetBase + 4;
  crdxSpreadSheetCross = crdxSpreadSheetBase + 5;

  // dxPDFViewer
  crdxPDFViewerBase = 5200;
  crdxPDFViewerContext = crdxPDFViewerBase + 1;
  crdxPDFViewerCross = crdxPDFViewerBase + 2;

    // cxVerticalGrid
  crcxInspectorInsert   = -1228;
  crcxInspectorAddChild = -1229;
  crcxInspectorAdd      = -1230;
  crcxInspectorHide     = -1241;
  crcxInspectorNoDrag   = -1242;

    // dxTree
  crdxTreeDrag          = -1011;

    // dxFlowChart
  crFlChartZoomIn       = 2001;
  crFlChartZoomOut      = 2002;

    // dxMasterView
  crdxMasterViewMirror = 1501;
  crdxMasterViewFullScroll = 1502;
  crdxMasterViewHorScroll = 1503;
  crdxMasterViewVerScroll = 1504;
  crdxMasterViewUpScroll = 1505;
  crdxMasterViewRightScroll = 1506;
  crdxMasterViewDownScroll = 1507;
  crdxMasterViewLeftScroll = 1508;
  crdxMasterViewHorSize = 1509;
  crdxMasterViewVerSize = 1512;
  crdxMasterViewRemove = 1510;
  crdxMasterViewNoDrop = 1511;


implementation

end.

