{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetActions;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, StrUtils, Classes, Controls, Graphics, Math, StdCtrls, Generics.Collections, Generics.Defaults,
  dxCore, cxClasses, cxGraphics, cxGeometry, dxActions, dxPrinting, dxHashUtils, cxRichEdit, dxGallery,
  dxSpreadSheetCore, dxSpreadSheetCoreHelpers, dxSpreadSheetGraphics, dxSpreadSheetContainers, dxSpreadSheetStyles,
  dxSpreadSheetPrinting, dxSpreadSheetFunctions;

type

  { TdxSpreadSheetAction }

  TdxSpreadSheetAction = class(TdxCustomAction)
  strict private
    FActiveCellStyle: TdxSpreadSheetCellStyle;
    FActiveSelection: TdxSpreadSheetTableViewSelection;
    FActiveTable: TdxSpreadSheetTableView;

    function GetActiveCellStyle: TdxSpreadSheetCellStyle;
    function GetControl: TdxCustomSpreadSheet; inline;
    function GetHistory: TdxSpreadSheetHistory; inline;
  protected
    FIsContainerSelectionSupported: Boolean;
    FIsSelectionNeeded: Boolean;

    procedure ProcessSelectedCellStyles(AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
    procedure SetControl(Value: TdxCustomSpreadSheet); reintroduce;
    //
    procedure DoExecute; virtual;
    procedure DoResetState; override;
    procedure DoUpdateState; override;
    function IsEnabled: Boolean; virtual;
    procedure UpdateControl(Target: TObject); override;
    procedure UpdateReferences;
    procedure UpdateState; override;
    //
    property ActiveCellStyle: TdxSpreadSheetCellStyle read GetActiveCellStyle;
    property ActiveSelection: TdxSpreadSheetTableViewSelection read FActiveSelection;
    property ActiveTable: TdxSpreadSheetTableView read FActiveTable;
    property Control: TdxCustomSpreadSheet read GetControl write SetControl;
    property History: TdxSpreadSheetHistory read GetHistory;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  { TdxSpreadSheetCustomEditAction }

  TdxSpreadSheetCustomEditAction = class(TdxSpreadSheetAction)
  protected
    function EnabledWhenEditing: Boolean; virtual;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetCustomContainerAction }

  TdxSpreadSheetCustomContainerAction = class(TdxSpreadSheetCustomEditAction)
  protected
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetCustomEditCellsAction }

  TdxSpreadSheetCustomEditCellsAction = class(TdxSpreadSheetCustomEditAction)
  protected
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetCustomFormatCellsAction }

  TdxSpreadSheetCustomFormatCellsAction = class(TdxSpreadSheetCustomEditCellsAction)
  protected
    function GetEdit(out AEdit: TcxCustomRichEdit): Boolean;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetValueAction }

  TdxSpreadSheetValueAction = class(TdxSpreadSheetCustomFormatCellsAction, IdxActionValue)
  strict private
    FValue: Variant;
  protected
    procedure DoActionValueChanged(const AValue: Variant); override;
    // IdxActionValue
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
    class function IsValueEquals(const AValue1, AValue2: Variant): Boolean; virtual;
  public
    property Value: Variant read GetValue write SetValue;
  end;

  { TdxSpreadSheetCopySelection }

  TdxSpreadSheetCopySelection = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCutSelection }

  TdxSpreadSheetCutSelection = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetPasteSelection }

  TdxSpreadSheetPasteSelection = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetDecreaseFontSize }

  TdxSpreadSheetDecreaseFontSize = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    function DecreaseSize(ASize: Integer): Integer;
    procedure DoExecute; override;
    function EnabledWhenEditing: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetIncreaseFontSize }

  TdxSpreadSheetIncreaseFontSize = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
    function EnabledWhenEditing: Boolean; override;
    function IncreaseSize(ASize: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomToggleFontStyleAction }

  TdxSpreadSheetCustomToggleFontStyleAction = class(TdxSpreadSheetCustomFormatCellsAction)
  private
    function GetFontStyle: TFontStyles;
    procedure SetFontStyle(AValue: TFontStyles);
  protected
    FStyle: TFontStyle;

    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function EnabledWhenEditing: Boolean; override;
    procedure DoResetState; override;

    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetToggleFontBold }

  TdxSpreadSheetToggleFontBold = class(TdxSpreadSheetCustomToggleFontStyleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetToggleFontItalic }

  TdxSpreadSheetToggleFontItalic = class(TdxSpreadSheetCustomToggleFontStyleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetToggleFontStrikeout }

  TdxSpreadSheetToggleFontStrikeout = class(TdxSpreadSheetCustomToggleFontStyleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetToggleFontUnderline }

  TdxSpreadSheetToggleFontUnderline = class(TdxSpreadSheetCustomToggleFontStyleAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomLineStyleBordersAction }

  TdxSpreadSheetCustomLineStyleBordersAction = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure SetEnumCellBottomBorder(ACellStyle: TdxSpreadSheetCellStyle;
      ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
    procedure SetEnumCellLeftBorder(ACellStyle: TdxSpreadSheetCellStyle;
      ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
    procedure SetEnumCellRightBorder(ACellStyle: TdxSpreadSheetCellStyle;
      ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
    procedure SetEnumCellTopBorder(ACellStyle: TdxSpreadSheetCellStyle;
      ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
  end;

  { TdxSpreadSheetBordersAll }

  TdxSpreadSheetBordersAll = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersBottom }

  TdxSpreadSheetBordersBottom = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersBottomDouble }

  TdxSpreadSheetBordersBottomDouble = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersBottomThick }

  TdxSpreadSheetBordersBottomThick = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersLeft }

  TdxSpreadSheetBordersLeft = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersMore }

  TdxSpreadSheetBordersMore = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersNone }

  TdxSpreadSheetBordersNone = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersOutside }

  TdxSpreadSheetBordersOutside = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersRight }

  TdxSpreadSheetBordersRight = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersOutsideThick }

  TdxSpreadSheetBordersOutsideThick = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersTopAndBottom }

  TdxSpreadSheetBordersTopAndBottom = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersTopAndBottomDouble }

  TdxSpreadSheetBordersTopAndBottomDouble = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersTopAndBottomThick }

  TdxSpreadSheetBordersTopAndBottomThick = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetBordersTop }

  TdxSpreadSheetBordersTop = class(TdxSpreadSheetCustomLineStyleBordersAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomAlignHorizontalAction }

  TdxSpreadSheetCustomAlignHorizontalAction = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    FAlignHorizontal: TdxSpreadSheetDataAlignHorz;
    //
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetAlignHorizontalCenter }

  TdxSpreadSheetAlignHorizontalCenter = class(TdxSpreadSheetCustomAlignHorizontalAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetAlignHorizontalLeft }

  TdxSpreadSheetAlignHorizontalLeft = class(TdxSpreadSheetCustomAlignHorizontalAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetAlignHorizontalRight }

  TdxSpreadSheetAlignHorizontalRight = class(TdxSpreadSheetCustomAlignHorizontalAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomAlignVerticalAction }

  TdxSpreadSheetCustomAlignVerticalAction = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    FAlignVertical: TdxSpreadSheetDataAlignVert;

    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetAlignVerticalBottom }

  TdxSpreadSheetAlignVerticalBottom = class(TdxSpreadSheetCustomAlignVerticalAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetAlignVerticalCenter }

  TdxSpreadSheetAlignVerticalCenter = class(TdxSpreadSheetCustomAlignVerticalAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetAlignVerticalTop }

  TdxSpreadSheetAlignVerticalTop = class(TdxSpreadSheetCustomAlignVerticalAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetTextIndentDecrease }

  TdxSpreadSheetTextIndentDecrease = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetTextIndentIncrease }

  TdxSpreadSheetTextIndentIncrease = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetTextWrap }

  TdxSpreadSheetTextWrap = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetMergeCells }

  TdxSpreadSheetMergeCells = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetMergeCellsAcross }

  TdxSpreadSheetMergeCellsAcross = class(TdxSpreadSheetMergeCells)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetMergeCellsAndCenter }

  TdxSpreadSheetMergeCellsAndCenter = class(TdxSpreadSheetMergeCells)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUnmergeCells }

  TdxSpreadSheetUnmergeCells = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetInsertColumns }

  TdxSpreadSheetInsertColumns = class(TdxSpreadSheetCustomEditAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetInsertRows }

  TdxSpreadSheetInsertRows = class(TdxSpreadSheetCustomEditAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetInsertSheet }

  TdxSpreadSheetInsertSheet = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetDeleteColumns }

  TdxSpreadSheetDeleteColumns = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetDeleteRows }

  TdxSpreadSheetDeleteRows = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetDeleteSheet }

  TdxSpreadSheetDeleteSheet = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetAutoFitColumnWidth }

  TdxSpreadSheetAutoFitColumnWidth = class(TdxSpreadSheetCustomEditAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetAutoFitRowHeight }

  TdxSpreadSheetAutoFitRowHeight = class(TdxSpreadSheetCustomEditAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomChangeTableItemsVisibilityAction }

  TdxSpreadSheetCustomChangeTableItemsVisibilityAction = class(TdxSpreadSheetAction)
  protected
    procedure ChangeCellsVisibility(ACollection: TdxSpreadSheetTableItems;
      const AFirstIndex, ALastIndex: Integer; const AValue: Boolean);
  end;

  { TdxSpreadSheetHideColumns }

  TdxSpreadSheetHideColumns = class(TdxSpreadSheetCustomChangeTableItemsVisibilityAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetHideRows }

  TdxSpreadSheetHideRows = class(TdxSpreadSheetCustomChangeTableItemsVisibilityAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetHideSheet }

  TdxSpreadSheetHideSheet = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUnhideColumns }

  TdxSpreadSheetUnhideColumns = class(TdxSpreadSheetHideColumns)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUnhideRows }

  TdxSpreadSheetUnhideRows = class(TdxSpreadSheetHideRows)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUnhideSheet }

  TdxSpreadSheetUnhideSheet = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetClearAll }

  TdxSpreadSheetClearAll = class(TdxSpreadSheetCustomEditCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetClearContents }

  TdxSpreadSheetClearContents = class(TdxSpreadSheetCustomEditCellsAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetClearFormats }

  TdxSpreadSheetClearFormats = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomSortAction }

  TdxSpreadSheetCustomSortAction = class(TdxSpreadSheetCustomEditCellsAction)
  protected
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetSortAscending }

  TdxSpreadSheetSortAscending = class(TdxSpreadSheetCustomSortAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetSortDescending }

  TdxSpreadSheetSortDescending = class(TdxSpreadSheetCustomSortAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetGroupColumns }

  TdxSpreadSheetGroupColumns = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetGroupRows }

  TdxSpreadSheetGroupRows = class(TdxSpreadSheetGroupColumns)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUngroupColumns }

  TdxSpreadSheetUngroupColumns = class(TdxSpreadSheetGroupColumns)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUngroupRows }

  TdxSpreadSheetUngroupRows = class(TdxSpreadSheetGroupColumns)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetDeleteComments }

  TdxSpreadSheetDeleteComments = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomCommentAction }

  TdxSpreadSheetCustomCommentAction = class(TdxSpreadSheetCustomContainerAction)
  protected
    function GetContainer: TdxSpreadSheetCommentContainer;
  end;

  { TdxSpreadSheetEditComment }

  TdxSpreadSheetEditComment = class(TdxSpreadSheetCustomCommentAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetNewComment }

  TdxSpreadSheetNewComment = class(TdxSpreadSheetCustomCommentAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomCommentNavigationAction }

  TdxSpreadSheetCustomCommentNavigationAction = class(TdxSpreadSheetCustomCommentAction)
  protected
    FGoForward: Boolean;

    procedure DoExecute; override;
    function GetNextContainer(out AContainer: TdxSpreadSheetContainer): Boolean;
    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetNextComment }

  TdxSpreadSheetNextComment = class(TdxSpreadSheetCustomCommentNavigationAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetPreviousComment }

  TdxSpreadSheetPreviousComment = class(TdxSpreadSheetCustomCommentNavigationAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetShowHideComments }

  TdxSpreadSheetShowHideComments = class(TdxSpreadSheetCustomCommentAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetZoomDefault }

  TdxSpreadSheetZoomDefault = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetZoomIn }

  TdxSpreadSheetZoomIn = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetZoomOut }

  TdxSpreadSheetZoomOut = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetFreezeFirstColumn }

  TdxSpreadSheetFreezeFirstColumn = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetFreezePanes }

  TdxSpreadSheetFreezePanes = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetFreezeTopRow }

  TdxSpreadSheetFreezeTopRow = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUnfreezePanes }

  TdxSpreadSheetUnfreezePanes = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetChangeFillColor }

  TdxSpreadSheetChangeFillColor = class(TdxSpreadSheetValueAction, IdxActionColorValue)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetChangeFontColor }

  TdxSpreadSheetChangeFontColor = class(TdxSpreadSheetValueAction, IdxActionColorValue)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function EnabledWhenEditing: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetChangeFontName }

  TdxSpreadSheetChangeFontName = class(TdxSpreadSheetValueAction, IdxActionFontNameValue)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function EnabledWhenEditing: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetChangeFontSize }

  TdxSpreadSheetChangeFontSize = class(TdxSpreadSheetValueAction, IdxActionFontSizeValue)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    function EnabledWhenEditing: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetRedo }

  TdxSpreadSheetRedo = class(TdxSpreadSheetCustomEditAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetUndo }

  TdxSpreadSheetUndo = class(TdxSpreadSheetCustomEditAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetNewDocument }

  TdxSpreadSheetNewDocument = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetOpenDocument }

  TdxSpreadSheetOpenDocument = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetSaveDocumentAs }

  TdxSpreadSheetSaveDocumentAs = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetFindAndReplace }

  TdxSpreadSheetFindAndReplace = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetShowHyperlinkEditor }

  TdxSpreadSheetShowHyperlinkEditor = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetInsertPicture }

  TdxSpreadSheetInsertPicture = class(TdxSpreadSheetCustomContainerAction)
  strict private
    function AddPictureContainer(AWidth, AHeight: Integer): TdxSpreadSheetPictureContainer;
    procedure DoInsertPictureFromFile(const AFileName: string);
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetProtectSheet }

  TdxSpreadSheetProtectSheet = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetProtectWorkbook }

  TdxSpreadSheetProtectWorkbook = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    procedure DoUpdateState; override;
    procedure DoResetState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetShowPageSetupForm }

  TdxSpreadSheetShowPageSetupForm = class(TdxCustomShowPageSetupFormAction)
  protected
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetShowPrintForm }

  TdxSpreadSheetShowPrintForm = class(TdxCustomShowPrintFormAction)
  protected
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetShowPrintPreviewForm }

  TdxSpreadSheetShowPrintPreviewForm = class(TdxCustomShowPrintPreviewFormAction)
  protected
    function GetControlClass: TWinControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomPrintingAction }

  TdxSpreadSheetCustomPrintingAction = class(TdxSpreadSheetAction)
  strict private
    function GetOptionsPrint: TdxSpreadSheetTableViewOptionsPrint;
  protected
    procedure DoExecute; override;
    function DoExecuteCore: Boolean; virtual; abstract;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    //
    property OptionsPrint: TdxSpreadSheetTableViewOptionsPrint read GetOptionsPrint;
  end;

  { TdxSpreadSheetClearPrintArea }

  TdxSpreadSheetClearPrintArea = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetSetPrintArea }

  TdxSpreadSheetSetPrintArea = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomPageSetupAction }

  TdxSpreadSheetCustomPageSetupAction = class(TdxSpreadSheetAction)
  protected
    procedure DoExecute; override;
    function GetPageIndex: Integer; virtual;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetPrintTitles }

  TdxSpreadSheetPrintTitles = class(TdxSpreadSheetCustomPageSetupAction)
  protected
    function GetPageIndex: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetInsertPageBreak }

  TdxSpreadSheetInsertPageBreak = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetRemovePageBreak }

  TdxSpreadSheetRemovePageBreak = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetResetAllPageBreaks }

  TdxSpreadSheetResetAllPageBreaks = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
    function IsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetSetLandscapePageOrientation }

  TdxSpreadSheetSetLandscapePageOrientation = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetSetPortraitPageOrientation }

  TdxSpreadSheetSetPortraitPageOrientation = class(TdxSpreadSheetCustomPrintingAction)
  protected
    function DoExecuteCore: Boolean; override;
    procedure DoUpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck default True;
  end;

  { TdxSpreadSheetCustomGalleryAction }

  TdxSpreadSheetCustomGalleryAction = class(TdxSpreadSheetAction,
    IdxActionGalleryClient,
    IdxLocalizerListener)
  protected
    function FindGalleryGroup(const AName: string): TdxCustomGalleryGroup; virtual;
    function FindGalleryGroupItem(AGroup: TdxCustomGalleryGroup; AActionIndex: Variant): TdxCustomGalleryItem; virtual;
    procedure UpdateGalleryContentResourceStrings; virtual;

    // IdxActionGalleryClient
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const AValue: Variant); virtual; abstract;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); virtual; abstract;

    // IdxLocalizerListener
    procedure TranslationChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxSpreadSheetPaperSizeGallery }

  TdxSpreadSheetPaperSizeGallery = class(TdxSpreadSheetCustomGalleryAction)
  protected
    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetMorePaperSizes }

  TdxSpreadSheetMorePaperSizes = class(TdxSpreadSheetCustomPageSetupAction)
  protected
    function GetPageIndex: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetMorePageMargins }

  TdxSpreadSheetMorePageMargins = class(TdxSpreadSheetCustomPageSetupAction)
  protected
    function GetPageIndex: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetPageMarginsGallery }

  TdxSpreadSheetPageMarginsGallery = class(TdxSpreadSheetCustomGalleryAction)
  protected type
    TMarginsPart = (mpTop, mpBottom, mpLeft, mpRight, mpHeader, mpFooter);
    TMarginsSet = array [TMarginsPart] of Double;
    TPredefinedMarginsSet = (pmsNormal, pmsWide, pmsNarrow);
  protected const
    CPredefinedNarrowMarginsSet: TMarginsSet = (0.75, 0.75, 0.25, 0.25, 0.3, 0.3);
    CPredefinedNormalMarginsSet: TMarginsSet = (0.75, 0.75, 0.7, 0.7, 0.3, 0.3);
    CPredefinedWideMarginsSet: TMarginsSet = (1, 1, 1, 1, 0.5, 0.5);
  strict private
    function GetPredefinedMarginsSet(const APredefinedMarginsSet: TPredefinedMarginsSet): TMarginsSet;
    function GetPredefinedMarginsSetCaption(const APredefinedMarginsSet: TPredefinedMarginsSet): string;
    function GetPredefinedMarginsSetDescription(const APredefinedMarginsSet: TPredefinedMarginsSet): string;

    procedure ReadGalleryGroup(Reader: TReader);
    procedure WriteGalleryGroup(Writer: TWriter);
  protected
    FGalleryGroup: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxSpreadSheetAutoSumGallery }

  TdxSpreadSheetAutoSumGallery = class(TdxSpreadSheetCustomGalleryAction)
  protected type
    TFormula = (fSum, fAverage, fCountNumbers, fMax, fMin);
  strict private
    function GetFormulaCaption(const AFormula: TFormula): string;
    function GetFormulaDescription(const AFormula: TFormula): string;

    procedure ReadGalleryGroup(Reader: TReader);
    procedure WriteGalleryGroup(Writer: TWriter);
  protected
    FGalleryGroup: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxSpreadSheetCustomFormulasGalleryAction }

  TdxSpreadSheetCustomFormulasGalleryAction = class(TdxSpreadSheetCustomGalleryAction)
  strict private
    procedure ReadGalleryGroup(Reader: TReader);
    procedure WriteGalleryGroup(Writer: TWriter);
  protected
    FGalleryGroup: TdxCustomGalleryGroup;

    function GetTypeID: TdxSpreadSheetFunctionType; virtual; abstract;

    procedure DefineProperties(Filer: TFiler); override;
    function GetDefaultCaption: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxSpreadSheetFinancialFormulasGallery }

  TdxSpreadSheetFinancialFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetLogicalFormulasGallery }

  TdxSpreadSheetLogicalFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetTextFormulasGallery }

  TdxSpreadSheetTextFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetDateAndTimeFormulasGallery }

  TdxSpreadSheetDateAndTimeFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetLookupAndReferenceFormulasGallery }

  TdxSpreadSheetLookupAndReferenceFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetMathAndTrigFormulasGallery }

  TdxSpreadSheetMathAndTrigFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetStatisticalFormulasGallery }

  TdxSpreadSheetStatisticalFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetInformationFormulasGallery }

  TdxSpreadSheetInformationFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCompatibilityFormulasGallery }

  TdxSpreadSheetCompatibilityFormulasGallery = class(TdxSpreadSheetCustomFormulasGalleryAction)
  protected
    function GetTypeID: TdxSpreadSheetFunctionType; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetPageOrientationGallery }

  TdxSpreadSheetPageOrientationGallery = class(TdxSpreadSheetCustomGalleryAction)
  strict private
    function GetPageOrientationCaption(AOrientation: TdxSpreadSheetTableViewOptionsPrintPageOrientation): string;

    procedure ReadGalleryGroup(Reader: TReader);
    procedure WriteGalleryGroup(Writer: TWriter);
  protected
    FGalleryGroup: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

    // IdxActionGalleryClient
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

implementation

uses
  Dialogs, ExtDlgs, dxGDIPlusClasses, dxSpreadSheetActionsStrs, dxSpreadSheetTypes, dxSpreadSheetFormatCellsDialog,
  dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetUnhideSheetDialog, dxSpreadSheetFindAndReplaceDialog,
  dxSpreadSheetEditHyperlinkDialog, dxSpreadSheetCoreStrs, dxSpreadSheetCoreHistory, dxSpreadSheetUtils,
  dxSpreadSheetPageSetupDialog, dxPrintUtils, dxMeasurementUnits, dxSpreadSheetDialogStrs, dxTypeHelpers,
  dxSpreadSheetFormulasHelpers, cxEdit, Variants, dxSpreadSheetInplaceEdit, dxSpreadSheetFunctionsStrs;

type
  TdxSpreadSheetTableItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TEditingControllerAccess = class(TcxCustomEditingController);
  TdxCustomGalleryItemAccess = class(TdxCustomGalleryItem);

{ TdxSpreadSheetAction }

constructor TdxSpreadSheetAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsContainerSelectionSupported := False;
  FIsSelectionNeeded := True;
end;

procedure TdxSpreadSheetAction.ExecuteTarget(Target: TObject);
begin
  UpdateControl(Target);
  if Control <> nil then
    DoExecute;
end;

function TdxSpreadSheetAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (inherited HandlesTarget(Target) or (Target is TdxCustomSpreadSheet)) and
    (not NeedControlFocus or TdxCustomSpreadSheet(Target).Focused);
end;

procedure TdxSpreadSheetAction.ProcessSelectedCellStyles(AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
var
  AEnumCellStylesHelper: TdxSpreadSheetTableViewEnumCellStylesHelper;
  AArea: TRect;
  AAreaIndex: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryFormatCellAction);
  try
    ActiveTable.BeginUpdate;
    try
      AEnumCellStylesHelper := TdxSpreadSheetTableViewEnumCellStylesHelper.Create(ActiveTable);
      try
        for AAreaIndex := 0 to ActiveSelection.Count - 1 do
        begin
          AArea := ActiveSelection.Items[AAreaIndex].Rect;
          AEnumCellStylesHelper.PrepareToSave(AArea);
          AEnumCellStylesHelper.ProcessArea(AArea, AProcRef);
        end;
      finally
        AEnumCellStylesHelper.Free;
      end;
      TdxSpreadSheetTableViewAccess(ActiveTable).Pack;
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

procedure TdxSpreadSheetAction.SetControl(Value: TdxCustomSpreadSheet);
begin
  inherited SetControl(Value);
end;

procedure TdxSpreadSheetAction.DoExecute;
begin
  //do nothing
end;

procedure TdxSpreadSheetAction.DoUpdateState;
begin
  Enabled := IsEnabled and Control.CanFocusEx;
end;

function TdxSpreadSheetAction.IsEnabled: Boolean;
begin
  Result := not FIsSelectionNeeded or ((ActiveSelection.Count > 0) and
    (FIsContainerSelectionSupported or (ActiveSelection.FocusedContainer = nil)));
end;

procedure TdxSpreadSheetAction.DoResetState;
begin
  Enabled := True;
end;

procedure TdxSpreadSheetAction.UpdateControl(Target: TObject);
begin
  if Target is TdxCustomSpreadSheet then
    Control := TdxCustomSpreadSheet(Target);
end;

procedure TdxSpreadSheetAction.UpdateReferences;
begin
  FActiveTable := nil;
  FActiveSelection := nil;
  FActiveCellStyle := nil;
  if Control <> nil then
  begin
    FActiveTable := Control.ActiveSheetAsTable;
    if FActiveTable <> nil then
      FActiveSelection := FActiveTable.Selection;
  end;
end;

procedure TdxSpreadSheetAction.UpdateState;
begin
  UpdateReferences;
  inherited UpdateState;
end;

function TdxSpreadSheetAction.GetActiveCellStyle: TdxSpreadSheetCellStyle;
begin
  if FActiveCellStyle = nil then
  begin
    if (ActiveSelection <> nil) and (ActiveSelection.FocusedContainer = nil) then
      FActiveCellStyle := TdxSpreadSheetTableViewAccess(ActiveTable).GetFocusedCellStyle
    else
      FActiveCellStyle := Control.DefaultCellStyle;
  end;
  Result := FActiveCellStyle;
end;

function TdxSpreadSheetAction.GetControl: TdxCustomSpreadSheet;
begin
  Result := TdxCustomSpreadSheet(inherited Control);
end;

function TdxSpreadSheetAction.GetHistory: TdxSpreadSheetHistory;
begin
  Result := Control.History;
end;

{ TdxSpreadSheetCustomEditAction }

function TdxSpreadSheetCustomEditAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.OptionsBehavior.Editing and
    (not ActiveTable.IsEditing or EnabledWhenEditing);
end;

function TdxSpreadSheetCustomEditAction.EnabledWhenEditing: Boolean;
begin
  Result := False;
end;

{ TdxSpreadSheetCustomContainerAction }

function TdxSpreadSheetCustomContainerAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanEditContainers;
end;

{ TdxSpreadSheetCustomEditCellsAction }

function TdxSpreadSheetCustomEditCellsAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (not ActiveTable.OptionsProtection.&Protected or
    (TdxSpreadSheetTableViewAccess(ActiveTable).GetLockedStateOfCellsInArea(ActiveSelection.Area) = cbUnchecked));
end;

{ TdxSpreadSheetCustomFormatCellsAction }

function TdxSpreadSheetCustomFormatCellsAction.GetEdit(out AEdit: TcxCustomRichEdit): Boolean;
begin
  Result := TdxSpreadSheetTableViewEditingController.GetActualInplaceEdit(ActiveTable, TdxSpreadSheetCustomInplaceEdit(AEdit));
end;

function TdxSpreadSheetCustomFormatCellsAction.IsEnabled: Boolean;
var
  AEdit: TcxCustomRichEdit;
begin
  Result := inherited IsEnabled and ActiveTable.OptionsProtection.ActualAllowFormatCells and
    (not ActiveTable.IsEditing or EnabledWhenEditing and dxSpreadSheetTextService.IsRTFSupported and GetEdit(AEdit));
end;

{ TdxSpreadSheetValueAction }

procedure TdxSpreadSheetValueAction.DoActionValueChanged(const AValue: Variant);
begin
  if not IsLocked then
    FValue := AValue;
  inherited DoActionValueChanged(AValue);
end;

function TdxSpreadSheetValueAction.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TdxSpreadSheetValueAction.SetValue(const AValue: Variant);
begin
  if not IsValueEquals(FValue, AValue) then
  begin
    FValue := AValue;
    if Enabled and not Execute then
      DoExecute;
  end;
end;

class function TdxSpreadSheetValueAction.IsValueEquals(const AValue1, AValue2: Variant): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TdxSpreadSheetCopySelection }

constructor TdxSpreadSheetCopySelection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionCopySelectionCaption;
  FDefaultHintResString := @sdxSpreadSheetActionCopySelectionHint;
  FDefaultImageNameInIconLibrary := 'Edit\Copy.png';
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetCopySelection.DoExecute;
var
  AEdit: TdxSpreadSheetCustomInplaceEdit;
begin
  if TdxSpreadSheetTableViewEditingController.GetActualInplaceEdit(ActiveTable, AEdit) then
    AEdit.CopyToClipboard
  else
    ActiveTable.CopyToClipboard;
end;

function TdxSpreadSheetCopySelection.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (ActiveTable.CanCopyToClipboard or ActiveTable.IsEditing);
end;

{ TdxSpreadSheetCutSelection }

constructor TdxSpreadSheetCutSelection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionCutSelectionCaption;
  FDefaultHintResString := @sdxSpreadSheetActionCutSelectionHint;
  FDefaultImageNameInIconLibrary := 'Edit\Cut.png';
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetCutSelection.DoExecute;
var
  AEdit: TdxSpreadSheetCustomInplaceEdit;
begin
  if TdxSpreadSheetTableViewEditingController.GetActualInplaceEdit(ActiveTable, AEdit) then
    AEdit.CutToClipboard
  else
    ActiveTable.CutToClipboard;
end;

function TdxSpreadSheetCutSelection.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (ActiveTable.CanCutToClipboard or ActiveTable.IsEditing);
end;

{ TdxSpreadSheetPasteSelection }

constructor TdxSpreadSheetPasteSelection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionPasteSelectionCaption;
  FDefaultHintResString := @sdxSpreadSheetActionPasteSelectionHint;
  FDefaultImageNameInIconLibrary := 'Edit\Paste.png';
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetPasteSelection.DoExecute;
var
  AEdit: TdxSpreadSheetCustomInplaceEdit;
begin
  if TdxSpreadSheetTableViewEditingController.GetActualInplaceEdit(ActiveTable, AEdit) then
    AEdit.PasteFromClipboard
  else
    ActiveTable.PasteFromClipboard;
end;

function TdxSpreadSheetPasteSelection.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (ActiveTable.CanPasteFromClipboard or ActiveTable.IsEditing);
end;

{ TdxSpreadSheetDecreaseFontSize }

constructor TdxSpreadSheetDecreaseFontSize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionDecreaseFontSizeCaption;
  FDefaultHintResString := @sdxSpreadSheetActionDecreaseFontSizeHint;
  FDefaultImageNameInIconLibrary := 'Format\FontSizeDecrease.png';
end;


function TdxSpreadSheetDecreaseFontSize.DecreaseSize(ASize: Integer): Integer;
var
  I: Integer;
begin
  Result := ASize;
  for I := dxDefaultFontSizeCount - 1 downto 0 do
    if ASize > dxDefaultFontSizes[I] then
    begin
      Result := dxDefaultFontSizes[I];
      Break;
    end;
end;

procedure TdxSpreadSheetDecreaseFontSize.DoExecute;
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    AEdit.SelAttributes.Size := DecreaseSize(AEdit.SelAttributes.Size)
  else
    ProcessSelectedCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Size := DecreaseSize(ACellStyle.Font.Size);
      end);
end;

function TdxSpreadSheetDecreaseFontSize.EnabledWhenEditing: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetIncreaseFontSize }

constructor TdxSpreadSheetIncreaseFontSize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionIncreaseFontSizeCaption;
  FDefaultHintResString := @sdxSpreadSheetActionIncreaseFontSizeHint;
  FDefaultImageNameInIconLibrary := 'Format\FontSizeIncrease.png';
end;

procedure TdxSpreadSheetIncreaseFontSize.DoExecute;
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    AEdit.SelAttributes.Size := IncreaseSize(AEdit.SelAttributes.Size)
  else
    ProcessSelectedCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Size := IncreaseSize(ACellStyle.Font.Size);
      end);
end;

function TdxSpreadSheetIncreaseFontSize.EnabledWhenEditing: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetIncreaseFontSize.IncreaseSize(ASize: Integer): Integer;
var
  I: Integer;
begin
  Result := ASize;
  for I := 0 to dxDefaultFontSizeCount - 1 do
    if ASize < dxDefaultFontSizes[I] then
    begin
      Result := dxDefaultFontSizes[I];
      Break;
    end;
end;

{ TdxSpreadSheetCustomToggleFontStyleAction }

procedure TdxSpreadSheetCustomToggleFontStyleAction.DoExecute;
begin
  if ActiveTable.IsEditing then
  begin
    if Checked then
      FontStyle := FontStyle + [FStyle]
    else
      FontStyle :=  FontStyle - [FStyle];
  end
  else
    ProcessSelectedCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      var
        AFontStyles: TFontStyles;
      begin
        AFontStyles := ACellStyle.Font.Style;
        if Checked then
          Include(AFontStyles, FStyle)
        else
          Exclude(AFontStyles, FStyle);
        ACellStyle.Font.Style := AFontStyles;
      end);
end;

procedure TdxSpreadSheetCustomToggleFontStyleAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := FStyle in FontStyle;
end;

function TdxSpreadSheetCustomToggleFontStyleAction.EnabledWhenEditing: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetCustomToggleFontStyleAction.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

function TdxSpreadSheetCustomToggleFontStyleAction.GetFontStyle: TFontStyles;
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    Result := AEdit.SelAttributes.Style
  else
    Result := ActiveCellStyle.Font.Style;
end;

procedure TdxSpreadSheetCustomToggleFontStyleAction.SetFontStyle(AValue: TFontStyles);
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    AEdit.SelAttributes.Style := AValue
  else
    ActiveCellStyle.Font.Style := AValue;
end;


{ TdxSpreadSheetToggleFontBold }

constructor TdxSpreadSheetToggleFontBold.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionToggleFontBoldCaption;
  FDefaultHintResString := @sdxSpreadSheetActionToggleFontBoldHint;
  FDefaultImageNameInIconLibrary := 'Format\Bold.png';
  FStyle := fsBold;
end;

{ TdxSpreadSheetToggleFontItalic }

constructor TdxSpreadSheetToggleFontItalic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionToggleFontItalicCaption;
  FDefaultHintResString := @sdxSpreadSheetActionToggleFontItalicHint;
  FDefaultImageNameInIconLibrary := 'Format\Italic.png';
  FStyle := fsItalic;
end;

{ TdxSpreadSheetToggleFontStrikeout }

constructor TdxSpreadSheetToggleFontStrikeout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionToggleFontStrikeoutCaption;
  FDefaultHintResString := @sdxSpreadSheetActionToggleFontStrikeoutHint;
  FDefaultImageNameInIconLibrary := 'Format\Strikeout.png';
  FStyle := fsStrikeOut;
end;

{ TdxSpreadSheetToggleFontUnderline }

constructor TdxSpreadSheetToggleFontUnderline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionToggleFontUnderlineCaption;
  FDefaultHintResString := @sdxSpreadSheetActionToggleFontUnderlineHint;
  FDefaultImageNameInIconLibrary := 'Format\Underline.png';
  FStyle := fsUnderline;
end;

{ TdxSpreadSheetCustomLineStyleBordersAction }

procedure TdxSpreadSheetCustomLineStyleBordersAction.SetEnumCellBottomBorder(ACellStyle: TdxSpreadSheetCellStyle;
  ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
begin
  if (ARow < dxSpreadSheetMaxRowCount - 1) and not ActiveSelection.IsCellSelected(ARow + 1, AColumn) then
    ACellStyle.Borders[bBottom].Style := ABorderStyle;
end;

procedure TdxSpreadSheetCustomLineStyleBordersAction.SetEnumCellLeftBorder(ACellStyle: TdxSpreadSheetCellStyle;
  ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
begin
  if (AColumn = 0) or not ActiveSelection.IsCellSelected(ARow, AColumn - 1) then
    ACellStyle.Borders[bLeft].Style := ABorderStyle;
end;

procedure TdxSpreadSheetCustomLineStyleBordersAction.SetEnumCellRightBorder(ACellStyle: TdxSpreadSheetCellStyle;
  ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
begin
  if (AColumn < dxSpreadSheetMaxColumnCount - 1) and not ActiveSelection.IsCellSelected(ARow, AColumn + 1) then
    ACellStyle.Borders[bRight].Style := ABorderStyle;
end;

procedure TdxSpreadSheetCustomLineStyleBordersAction.SetEnumCellTopBorder(ACellStyle: TdxSpreadSheetCellStyle;
  ARow, AColumn: Integer; const AArea: TRect; const ABorderStyle: TdxSpreadSheetCellBorderStyle);
begin
  if (ARow = 0) or not ActiveSelection.IsCellSelected(ARow - 1, AColumn) then
    ACellStyle.Borders[bTop].Style := ABorderStyle;
end;

{ TdxSpreadSheetBordersAll }

constructor TdxSpreadSheetBordersAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersAllCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersAllHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BordersAll.png';
end;

procedure TdxSpreadSheetBordersAll.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      ACellStyle.Borders[bLeft].Style := sscbsThin;
      ACellStyle.Borders[bRight].Style := sscbsThin;
      ACellStyle.Borders[bTop].Style := sscbsThin;
      ACellStyle.Borders[bBottom].Style := sscbsThin;
    end);
end;

{ TdxSpreadSheetBordersBottom }

constructor TdxSpreadSheetBordersBottom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersBottomCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersBottomHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BorderBottom.png';
end;

procedure TdxSpreadSheetBordersBottom.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersBottomDouble }

constructor TdxSpreadSheetBordersBottomDouble.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersBottomDoubleCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersBottomDoubleHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\BorderBottomDouble.png';
end;

procedure TdxSpreadSheetBordersBottomDouble.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsDouble);
    end);
end;

{ TdxSpreadSheetBordersBottomThick }

constructor TdxSpreadSheetBordersBottomThick.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersBottomThickCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersBottomThickHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\BorderBottomThick.png';
end;

procedure TdxSpreadSheetBordersBottomThick.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsThick);
    end);
end;

{ TdxSpreadSheetBordersLeft }

constructor TdxSpreadSheetBordersLeft.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersLeftCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersLeftHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BorderLeft.png';
end;

procedure TdxSpreadSheetBordersLeft.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellLeftBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersMore }

constructor TdxSpreadSheetBordersMore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersMoreCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersMoreHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetBordersMore.DoExecute;
begin
  ShowFormatCellsDialog(ActiveTable, 3);
end;

{ TdxSpreadSheetBordersNone }

constructor TdxSpreadSheetBordersNone.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersNoneCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersNoneHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BorderNone.png';
end;

procedure TdxSpreadSheetBordersNone.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    var
      I: TcxBorder;
    begin
      for I := bLeft to bBottom do
        ACellStyle.Borders[I].Style := sscbsDefault;
    end);
end;

{ TdxSpreadSheetBordersOutside }

constructor TdxSpreadSheetBordersOutside.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersOutsideCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersOutsideHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BordersOutside.png';
end;

procedure TdxSpreadSheetBordersOutside.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
      SetEnumCellTopBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
      SetEnumCellLeftBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
      SetEnumCellRightBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersRight }

constructor TdxSpreadSheetBordersRight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersRightCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersRightHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BorderRight.png';
end;

procedure TdxSpreadSheetBordersRight.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellRightBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersOutsideThick }

constructor TdxSpreadSheetBordersOutsideThick.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersOutsideThickCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersOutsideThickHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\BordersOutsideThick.png';
end;

procedure TdxSpreadSheetBordersOutsideThick.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsThick);
      SetEnumCellTopBorder(ACellStyle, ARow, AColumn, AArea, sscbsThick);
      SetEnumCellLeftBorder(ACellStyle, ARow, AColumn, AArea, sscbsThick);
      SetEnumCellRightBorder(ACellStyle, ARow, AColumn, AArea, sscbsThick);
    end);
end;

{ TdxSpreadSheetBordersTopAndBottom }

constructor TdxSpreadSheetBordersTopAndBottom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersTopAndBottomCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersTopAndBottomHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\BorderTopAndBottom.png';
end;

procedure TdxSpreadSheetBordersTopAndBottom.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
      SetEnumCellTopBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersTopAndBottomDouble }

constructor TdxSpreadSheetBordersTopAndBottomDouble.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersTopAndBottomDoubleCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersTopAndBottomDoubleHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\BorderTopAndBottomDouble.png';
end;

procedure TdxSpreadSheetBordersTopAndBottomDouble.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsDouble);
      SetEnumCellTopBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersTopAndBottomThick }

constructor TdxSpreadSheetBordersTopAndBottomThick.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersTopAndBottomThickCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersTopAndBottomThickHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\BorderTopAndBottomThick.png';
end;

procedure TdxSpreadSheetBordersTopAndBottomThick.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellBottomBorder(ACellStyle, ARow, AColumn, AArea, sscbsThick);
      SetEnumCellTopBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetBordersTop }

constructor TdxSpreadSheetBordersTop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionBordersTopCaption;
  FDefaultHintResString := @sdxSpreadSheetActionBordersTopHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\BorderTop.png';
end;

procedure TdxSpreadSheetBordersTop.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      SetEnumCellTopBorder(ACellStyle, ARow, AColumn, AArea, sscbsThin);
    end);
end;

{ TdxSpreadSheetCustomAlignHorizontalAction }

procedure TdxSpreadSheetCustomAlignHorizontalAction.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      if Checked then
        ACellStyle.AlignHorz := FAlignHorizontal
      else
        ACellStyle.AlignHorz := ssahGeneral;
    end);
end;

procedure TdxSpreadSheetCustomAlignHorizontalAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := ActiveCellStyle.AlignHorz = FAlignHorizontal;
end;

procedure TdxSpreadSheetCustomAlignHorizontalAction.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

{ TdxSpreadSheetAlignHorizontalCenter }

constructor TdxSpreadSheetAlignHorizontalCenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionAlignHorizontalCenterCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAlignHorizontalCenterHint;
  FDefaultImageNameInIconLibrary := 'Format\AlignCenter.png';
  FAlignHorizontal := ssahCenter;
end;

{ TdxSpreadSheetAlignHorizontalLeft }

constructor TdxSpreadSheetAlignHorizontalLeft.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionAlignHorizontalLeftCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAlignHorizontalLeftHint;
  FDefaultImageNameInIconLibrary := 'Format\AlignLeft.png';
  FAlignHorizontal := ssahLeft;
end;

{ TdxSpreadSheetAlignHorizontalRight }

constructor TdxSpreadSheetAlignHorizontalRight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionAlignHorizontalRightCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAlignHorizontalRightHint;
  FDefaultImageNameInIconLibrary := 'Format\AlignRight.png';
  FAlignHorizontal := ssahRight;
end;

{ TdxSpreadSheetCustomAlignVerticalAction }

procedure TdxSpreadSheetCustomAlignVerticalAction.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      ACellStyle.AlignVert := FAlignVertical;
    end);
end;

procedure TdxSpreadSheetCustomAlignVerticalAction.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := ActiveCellStyle.AlignVert = FAlignVertical;
end;

procedure TdxSpreadSheetCustomAlignVerticalAction.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

{ TdxSpreadSheetAlignVerticalBottom }

constructor TdxSpreadSheetAlignVerticalBottom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionAlignVerticalBottomCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAlignVerticalBottomHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\AlignBottomCenter.png';
  FAlignVertical := ssavBottom;
end;

{ TdxSpreadSheetAlignVerticalCenter }

constructor TdxSpreadSheetAlignVerticalCenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionAlignVerticalCenterCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAlignVerticalCenterHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\AlignMiddleCenter.png';
  FAlignVertical := ssavCenter;
end;

{ TdxSpreadSheetAlignVerticalTop }

constructor TdxSpreadSheetAlignVerticalTop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionAlignVerticalTopCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAlignVerticalTopHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\AlignTopCenter.png';
  FAlignVertical := ssavTop;
end;

{ TdxSpreadSheetTextIndentDecrease }

constructor TdxSpreadSheetTextIndentDecrease.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionTextIndentDecreaseCaption;
  FDefaultHintResString := @sdxSpreadSheetActionTextIndentDecreaseHint;
  FDefaultImageNameInIconLibrary := 'Format\IndentDecrease.png';
end;

procedure TdxSpreadSheetTextIndentDecrease.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      if ACellStyle.AlignHorzIndent >=2 then
        ACellStyle.AlignHorzIndent := ACellStyle.AlignHorzIndent - 2
      else
        ACellStyle.AlignHorzIndent := 0;
    end);
end;

{ TdxSpreadSheetTextIndentIncrease }

constructor TdxSpreadSheetTextIndentIncrease.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionTextIndentIncreaseCaption;
  FDefaultHintResString := @sdxSpreadSheetActionTextIndentIncreaseHint;
  FDefaultImageNameInIconLibrary := 'Format\IndentIncrease.png';
end;

procedure TdxSpreadSheetTextIndentIncrease.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      ACellStyle.AlignHorzIndent := ACellStyle.AlignHorzIndent + 2;
    end);
end;

{ TdxSpreadSheetTextWrap }

constructor TdxSpreadSheetTextWrap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionTextWrapCaption;
  FDefaultHintResString := @sdxSpreadSheetActionTextWrapHint;
  FDefaultImageNameInIconLibrary := 'Arrange\WithTextWrapping_CenterRight.png';
end;

procedure TdxSpreadSheetTextWrap.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      ACellStyle.WordWrap := Checked;
    end);
end;

procedure TdxSpreadSheetTextWrap.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := ActiveCellStyle.WordWrap;
end;

procedure TdxSpreadSheetTextWrap.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

{ TdxSpreadSheetMergeCells }

constructor TdxSpreadSheetMergeCells.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionMergeCellsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionMergeCellsHint;
  FDefaultImageNameInIconLibrary := 'Alignment\MergeCells.png';
end;

procedure TdxSpreadSheetMergeCells.DoExecute;
begin
  ActiveTable.MergeSelected;
end;

function TdxSpreadSheetMergeCells.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanMergeSelected;
end;

{ TdxSpreadSheetMergeCellsAcross }

constructor TdxSpreadSheetMergeCellsAcross.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionMergeCellsAcrossCaption;
  FDefaultHintResString := @sdxSpreadSheetActionMergeCellsAcrossHint;
  FDefaultImageNameInIconLibrary := 'Alignment\MergeAcross.png';
end;

procedure TdxSpreadSheetMergeCellsAcross.DoExecute;
var
  ARow, I: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryMergeCellsAction);
  try
    ActiveTable.BeginUpdate;
    try
      for I := 0 to ActiveSelection.Count - 1 do
        for ARow := ActiveSelection[I].Top to ActiveSelection[I].Bottom do
          ActiveTable.MergedCells.Add(Rect(ActiveSelection[I].Left, ARow, ActiveSelection[I].Right, ARow));
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetMergeCellsAndCenter }

constructor TdxSpreadSheetMergeCellsAndCenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionMergeCellsAndCenterCaption;
  FDefaultHintResString := @sdxSpreadSheetActionMergeCellsAndCenterHint;
  FDefaultImageNameInIconLibrary := 'Alignment\MergeCenter.png';
end;

procedure TdxSpreadSheetMergeCellsAndCenter.DoExecute;
var
  I: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryMergeCellsAction);
  try
    ActiveTable.BeginUpdate;
    try
      ActiveTable.MergeSelected;
      for I := 0 to ActiveSelection.Count - 1 do
        ActiveTable.CreateCell(ActiveSelection[I].Top, ActiveSelection[I].Left).Style.AlignHorz := ssahCenter;
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetUnmergeCells }

constructor TdxSpreadSheetUnmergeCells.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUnmergeCellsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUnmergeCellsHint;
  FDefaultImageNameInIconLibrary := 'Alignment\UnmergeCells.png';
end;

procedure TdxSpreadSheetUnmergeCells.DoExecute;
begin
  History.BeginAction(TdxSpreadSheetHistoryMergeCellsAction);
  try
    ActiveTable.MergedCells.DeleteItemsInArea(ActiveSelection.Area);
  finally
    History.EndAction;
  end;
end;

function TdxSpreadSheetUnmergeCells.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanSplitSelected;
end;

{ TdxSpreadSheetInsertColumns }

constructor TdxSpreadSheetInsertColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionInsertColumnsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionInsertColumnsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\InsertColumns.png';
end;

procedure TdxSpreadSheetInsertColumns.DoExecute;
begin
  if ActiveSelection.Count <> 1 then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
  ActiveTable.InsertColumns(ActiveSelection.FocusedColumn, cxRectWidth(ActiveSelection.Area) + 1);
end;

function TdxSpreadSheetInsertColumns.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanInsert(cmShiftColumns);
end;

{ TdxSpreadSheetInsertRows }

constructor TdxSpreadSheetInsertRows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionInsertRowsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionInsertRowsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\InsertRows.png';
end;

procedure TdxSpreadSheetInsertRows.DoExecute;
begin
  if ActiveSelection.Count <> 1 then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
  ActiveTable.InsertRows(ActiveSelection.FocusedRow, cxRectHeight(ActiveSelection.Area) + 1);
end;

function TdxSpreadSheetInsertRows.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanInsert(cmShiftRows);
end;

{ TdxSpreadSheetInsertSheet }

constructor TdxSpreadSheetInsertSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionInsertSheetCaption;
  FDefaultHintResString := @sdxSpreadSheetActionInsertSheetHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\InsertSheet.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetInsertSheet.DoExecute;
begin
  Control.AddSheet.Index := ActiveTable.Index;
end;

function TdxSpreadSheetInsertSheet.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.OptionsProtection.ActualAllowChangeStructure;
end;

{ TdxSpreadSheetDeleteColumns }

constructor TdxSpreadSheetDeleteColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionDeleteColumnsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionDeleteColumnsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\DeleteSheetColumns.png';
end;

procedure TdxSpreadSheetDeleteColumns.DoExecute;
begin
  if ActiveSelection.Count <> 1 then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
  ActiveTable.DeleteColumns(ActiveSelection.FocusedColumn, cxRectWidth(ActiveSelection.Area) + 1);
end;

function TdxSpreadSheetDeleteColumns.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanDelete(cmShiftColumns);
end;

{ TdxSpreadSheetDeleteRows }

constructor TdxSpreadSheetDeleteRows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionDeleteRowsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionDeleteRowsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\DeleteSheetRows.png';
end;

procedure TdxSpreadSheetDeleteRows.DoExecute;
begin
  if ActiveSelection.Count <> 1 then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
  ActiveTable.DeleteRows(ActiveSelection.FocusedRow, cxRectHeight(ActiveSelection.Area) + 1);
end;

function TdxSpreadSheetDeleteRows.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanDelete(cmShiftRows);
end;

{ TdxSpreadSheetDeleteSheet }

constructor TdxSpreadSheetDeleteSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionDeleteSheetCaption;
  FDefaultHintResString := @sdxSpreadSheetActionDeleteSheetHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\DeleteSheet.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetDeleteSheet.DoExecute;
begin
  Control.ActiveSheet.Free;
end;

function TdxSpreadSheetDeleteSheet.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.VisibleSheetCount > 1) and
    Control.OptionsProtection.ActualAllowChangeStructure;
end;

{ TdxSpreadSheetAutoFitColumnWidth }

constructor TdxSpreadSheetAutoFitColumnWidth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionAutoFitColumnWidthCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAutoFitColumnWidthHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetAutoFitColumnWidth.DoExecute;
var
  AColumn: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    ActiveTable.BeginUpdate;
    try
      for AColumn := ActiveSelection.Area.Left to ActiveSelection.Area.Right do
        if ActiveTable.Columns[AColumn] <> nil then
          ActiveTable.Columns[AColumn].ApplyBestFit;
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

function TdxSpreadSheetAutoFitColumnWidth.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.OptionsProtection.ActualAllowResizeColumns;
end;

{ TdxSpreadSheetAutoFitRowHeight }

constructor TdxSpreadSheetAutoFitRowHeight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionAutoFitRowHeightCaption;
  FDefaultHintResString := @sdxSpreadSheetActionAutoFitRowHeightHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetAutoFitRowHeight.DoExecute;
var
  ARow: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    ActiveTable.BeginUpdate;
    try
      for ARow := ActiveSelection.Area.Top to ActiveSelection.Area.Bottom do
        if ActiveTable.Rows[ARow] <> nil then
          ActiveTable.Rows[ARow].ApplyBestFit;
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

function TdxSpreadSheetAutoFitRowHeight.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.OptionsProtection.ActualAllowResizeRows;
end;

{ TdxSpreadSheetCustomChangeTableItemsVisibilityAction }

procedure TdxSpreadSheetCustomChangeTableItemsVisibilityAction.ChangeCellsVisibility(
  ACollection: TdxSpreadSheetTableItems; const AFirstIndex, ALastIndex: Integer; const AValue: Boolean);
var
  I: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    ActiveTable.BeginUpdate;
    try
      if AValue then
      begin
        TdxSpreadSheetTableItemsAccess(ACollection).ForEach(
          procedure (AItem: TdxDynamicListItem)
          begin
            TdxSpreadSheetTableItem(AItem).Visible := True;
          end,
          AFirstIndex, ALastIndex);
      end
      else
        for I := AFirstIndex to ALastIndex do
          ACollection.CreateItem(I).Visible := False;
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetHideColumns }

constructor TdxSpreadSheetHideColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionHideColumnsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionHideColumnsHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetHideColumns.DoExecute;
begin
  ChangeCellsVisibility(ActiveTable.Columns, ActiveSelection.Area.Left, ActiveSelection.Area.Right, False);
end;

function TdxSpreadSheetHideColumns.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.OptionsProtection.ActualAllowResizeColumns;
end;

{ TdxSpreadSheetHideRows }

constructor TdxSpreadSheetHideRows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionHideRowsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionHideRowsHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetHideRows.DoExecute;
begin
  ChangeCellsVisibility(ActiveTable.Rows, ActiveSelection.Area.Top, ActiveSelection.Area.Bottom, False);
end;

function TdxSpreadSheetHideRows.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.OptionsProtection.ActualAllowResizeRows;
end;

{ TdxSpreadSheetHideSheet }

constructor TdxSpreadSheetHideSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionHideSheetCaption;
  FDefaultHintResString := @sdxSpreadSheetActionHideSheetHint;
  FDefaultImageNameInIconLibrary := '';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetHideSheet.DoExecute;
var
  AFirstVisibleSheet: Integer;
begin
  Control.ActiveSheet.Visible := False;
  AFirstVisibleSheet := Control.ActiveSheetIndex;
  while not Control.Sheets[AFirstVisibleSheet].Visible and (AFirstVisibleSheet < Control.SheetCount - 1) do
    Inc(AFirstVisibleSheet);
  while not Control.Sheets[AFirstVisibleSheet].Visible and (AFirstVisibleSheet > 0) do
    Dec(AFirstVisibleSheet);
  Control.ActiveSheetIndex := AFirstVisibleSheet;
end;

function TdxSpreadSheetHideSheet.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (Control.VisibleSheetCount > 1) and
    Control.OptionsProtection.ActualAllowChangeStructure;
end;

{ TdxSpreadSheetUnhideColumns }

constructor TdxSpreadSheetUnhideColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUnhideColumnsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUnhideColumnsHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetUnhideColumns.DoExecute;
begin
  ChangeCellsVisibility(ActiveTable.Columns, ActiveSelection.Area.Left, ActiveSelection.Area.Right, True);
end;

{ TdxSpreadSheetUnhideRows }

constructor TdxSpreadSheetUnhideRows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUnhideRowsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUnhideRowsHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetUnhideRows.DoExecute;
begin
  ChangeCellsVisibility(ActiveTable.Rows, ActiveSelection.Area.Top, ActiveSelection.Area.Bottom, True);
end;

{ TdxSpreadSheetUnhideSheet }

constructor TdxSpreadSheetUnhideSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUnhideSheetCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUnhideSheetHint;
  FDefaultImageNameInIconLibrary := '';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetUnhideSheet.DoExecute;
begin
  ShowUnhideSheetDialog(Control);
end;

function TdxSpreadSheetUnhideSheet.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.OptionsProtection.ActualAllowChangeStructure and
    (Control.SheetCount <> Control.VisibleSheetCount);
end;

{ TdxSpreadSheetClearAll }

constructor TdxSpreadSheetClearAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionClearAllCaption;
  FDefaultHintResString := @sdxSpreadSheetActionClearAllHint;
  FDefaultImageNameInIconLibrary := 'Actions\Clear.png';
end;

procedure TdxSpreadSheetClearAll.DoExecute;
var
  AAreaIndex: Integer;
begin
  History.BeginAction(TdxSpreadSheetHistoryClearCellsAction);
  try
    ActiveTable.BeginUpdate;
    try
      for AAreaIndex := 0 to ActiveSelection.Count - 1 do
        ActiveTable.ClearCells(ActiveSelection.Items[AAreaIndex].Rect);
    finally
      ActiveTable.EndUpdate;
    end;
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetClearContents }

constructor TdxSpreadSheetClearContents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionClearContentsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionClearContentsHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetClearContents.DoExecute;
begin
  ActiveTable.ClearCellValues;
end;

function TdxSpreadSheetClearContents.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanClearCells;
end;

{ TdxSpreadSheetClearFormats }

constructor TdxSpreadSheetClearFormats.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionClearFormatsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionClearFormatsHint;
  FDefaultImageNameInIconLibrary := 'Actions\ClearFormatting.png';
end;

procedure TdxSpreadSheetClearFormats.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      if ActiveTable.Cells[ARow, AColumn] <> nil then
        ActiveTable.Cells[ARow, AColumn].StyleHandle := Control.DefaultCellStyle.Handle;
    end);
end;

{ TdxSpreadSheetCustomSortAction }

function TdxSpreadSheetCustomSortAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanSort;
end;

{ TdxSpreadSheetSortAscending }

constructor TdxSpreadSheetSortAscending.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionSortAscendingCaption;
  FDefaultHintResString := @sdxSpreadSheetActionSortAscendingHint;
  FDefaultImageNameInIconLibrary := 'Data\SortAsc.png';
end;

procedure TdxSpreadSheetSortAscending.DoExecute;
begin
  ActiveTable.SortByColumnValues(ActiveSelection.Area, [soAscending], [ActiveSelection.Area.Left]);
end;

{ TdxSpreadSheetSortDescending }

constructor TdxSpreadSheetSortDescending.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionSortDescendingCaption;
  FDefaultHintResString := @sdxSpreadSheetActionSortDescendingHint;
  FDefaultImageNameInIconLibrary := 'Data\SortDesc.png';
end;

procedure TdxSpreadSheetSortDescending.DoExecute;
begin
  ActiveTable.SortByColumnValues(ActiveSelection.Area, [soDescending], [ActiveSelection.Area.Left]);
end;

{ TdxSpreadSheetGroupColumns }

constructor TdxSpreadSheetGroupColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionGroupColumnsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionGroupColumnsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\GroupRows.png';
end;

procedure TdxSpreadSheetGroupColumns.DoExecute;
begin
  if ActiveSelection.Count > 0 then
    ActiveTable.Columns.Groups.Add(ActiveSelection.Area.Left, ActiveSelection.Area.Right);
end;

function TdxSpreadSheetGroupColumns.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and not ActiveTable.Options.Protected;
end;

{ TdxSpreadSheetGroupRows }

constructor TdxSpreadSheetGroupRows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionGroupRowsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionGroupRowsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\GroupRows.png';
end;

procedure TdxSpreadSheetGroupRows.DoExecute;
begin
  if ActiveSelection.Count > 0 then
    ActiveTable.Rows.Groups.Add(ActiveSelection.Area.Top, ActiveSelection.Area.Bottom);
end;

{ TdxSpreadSheetUngroupColumns }

constructor TdxSpreadSheetUngroupColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUngroupColumnsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUngroupColumnsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\UngroupRows.png';
end;

procedure TdxSpreadSheetUngroupColumns.DoExecute;
begin
  if ActiveSelection.Count > 0 then
    ActiveTable.Columns.Groups.Delete(ActiveSelection.Area.Left, ActiveSelection.Area.Right);
end;

function TdxSpreadSheetUngroupColumns.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and not ActiveTable.Options.Protected;
end;

{ TdxSpreadSheetUngroupRows }

constructor TdxSpreadSheetUngroupRows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUngroupRowsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUngroupRowsHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\UngroupRows.png';
end;

procedure TdxSpreadSheetUngroupRows.DoExecute;
begin
  if ActiveSelection.Count > 0 then
    ActiveTable.Rows.Groups.Delete(ActiveSelection.Area.Top, ActiveSelection.Area.Bottom);
end;

function TdxSpreadSheetUngroupRows.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and not ActiveTable.Options.Protected;
end;

{ TdxSpreadSheetDeleteComments }

constructor TdxSpreadSheetDeleteComments.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionDeleteCommentsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionDeleteCommentsHint;
  FDefaultImageNameInIconLibrary := 'Comments\DeleteComment.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetDeleteComments.DoExecute;
begin
  ActiveTable.DeleteComments;
end;

function TdxSpreadSheetDeleteComments.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.CanDeleteComments;
end;

{ TdxSpreadSheetCustomCommentAction }

function TdxSpreadSheetCustomCommentAction.GetContainer: TdxSpreadSheetCommentContainer;
begin
  if ActiveSelection.FocusedContainer is TdxSpreadSheetCommentContainer then
    Result := TdxSpreadSheetCommentContainer(ActiveSelection.FocusedContainer)
  else
    Result := TdxSpreadSheetCommentContainer(ActiveTable.Containers.FindCommentContainer(
      ActiveSelection.FocusedRow, ActiveSelection.FocusedColumn));
end;

{ TdxSpreadSheetEditComment }

constructor TdxSpreadSheetEditComment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionEditCommentCaption;
  FDefaultHintResString := @sdxSpreadSheetActionEditCommentHint;
  FDefaultImageNameInIconLibrary := 'Comments\EditComment.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetEditComment.DoExecute;
begin
  ActiveTable.EditComment;
end;

function TdxSpreadSheetEditComment.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (GetContainer <> nil);
end;

{ TdxSpreadSheetNewComment }

constructor TdxSpreadSheetNewComment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionNewCommentCaption;
  FDefaultHintResString := @sdxSpreadSheetActionNewCommentHint;
  FDefaultImageNameInIconLibrary := 'Comments\InsertComment.png';
end;

procedure TdxSpreadSheetNewComment.DoExecute;
begin
  ActiveTable.EditComment;
end;

function TdxSpreadSheetNewComment.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (GetContainer = nil);
end;

{ TdxSpreadSheetCustomCommentNavigationAction }

procedure TdxSpreadSheetCustomCommentNavigationAction.DoExecute;
var
  AContainer: TdxSpreadSheetContainer;
begin
  if GetNextContainer(AContainer) then
    AContainer.Focused := True;
end;

function TdxSpreadSheetCustomCommentNavigationAction.GetNextContainer(out AContainer: TdxSpreadSheetContainer): Boolean;
var
  ACellRef: TPoint;
begin
  AContainer := ActiveTable.Controller.FocusedContainer;
  if AContainer is TdxSpreadSheetCommentContainer then
    ACellRef := Point(
      TdxSpreadSheetCommentContainer(AContainer).Cell.ColumnIndex,
      TdxSpreadSheetCommentContainer(AContainer).Cell.RowIndex)
  else
    ACellRef := Point(ActiveSelection.FocusedColumn, ActiveSelection.FocusedRow);

  Result := ActiveTable.Containers.FindNextCommentContainer(ACellRef.Y, ACellRef.X, AContainer, FGoForward);
end;

function TdxSpreadSheetCustomCommentNavigationAction.IsEnabled: Boolean;
var
  AContainer: TdxSpreadSheetContainer;
begin
  Result := inherited IsEnabled and not ActiveTable.IsEditing and GetNextContainer(AContainer);
end;

{ TdxSpreadSheetNextComment }

constructor TdxSpreadSheetNextComment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionNextCommentCaption;
  FDefaultHintResString := @sdxSpreadSheetActionNextCommentHint;
  FDefaultImageNameInIconLibrary := 'Comments\NextComment.png';
  FIsSelectionNeeded := False;
  FGoForward := True;
end;

{ TdxSpreadSheetPreviousComment }

constructor TdxSpreadSheetPreviousComment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionPreviousCommentCaption;
  FDefaultHintResString := @sdxSpreadSheetActionPreviousCommentHint;
  FDefaultImageNameInIconLibrary := 'Comments\PreviousComment.png';
  FIsSelectionNeeded := False;
  FGoForward := False;
end;

{ TdxSpreadSheetShowHideComments }

constructor TdxSpreadSheetShowHideComments.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionShowHideCommentsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionShowHideCommentsHint;
  FDefaultImageNameInIconLibrary := 'Comments\ShowHideComment.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetShowHideComments.DoExecute;
var
  AHasHidden: Boolean;
begin
  AHasHidden := False;
  ActiveTable.Containers.EnumCommentContainers(
    function (Container: TdxSpreadSheetContainer): Boolean
    begin
      AHasHidden := not Container.Visible;
      Result := Container.Visible;
    end);
  Checked := AHasHidden;
  ActiveTable.Containers.EnumCommentContainers(
    function (Container: TdxSpreadSheetContainer): Boolean
    begin
      Container.Visible := Checked;
      Result := True;
    end);
end;

function TdxSpreadSheetShowHideComments.IsEnabled: Boolean;
var
  AContainer: TdxSpreadSheetContainer;
begin
  Result := inherited IsEnabled and not ActiveTable.IsEditing and
    ActiveTable.Containers.FindNextCommentContainer(nil, AContainer);
end;

{ TdxSpreadSheetZoomDefault }

constructor TdxSpreadSheetZoomDefault.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionZoomDefaultCaption;
  FDefaultHintResString := @sdxSpreadSheetActionZoomDefaultHint;
  FDefaultImageNameInIconLibrary := 'Zoom\Zoom100.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetZoomDefault.DoExecute;
begin
  ActiveTable.Options.ZoomFactor := 100;
end;

{ TdxSpreadSheetZoomIn }

constructor TdxSpreadSheetZoomIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionZoomInCaption;
  FDefaultHintResString := @sdxSpreadSheetActionZoomInHint;
  FDefaultImageNameInIconLibrary := 'Zoom\ZoomIn.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetZoomIn.DoExecute;
begin
  ActiveTable.Options.ZoomFactor := ActiveTable.Options.ZoomFactor + 10;
end;

function TdxSpreadSheetZoomIn.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (ActiveTable.Options.ZoomFactor < dxSpreadSheetMaximumZoomFactor);
end;

{ TdxSpreadSheetZoomOut }

constructor TdxSpreadSheetZoomOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionZoomOutCaption;
  FDefaultHintResString := @sdxSpreadSheetActionZoomOutHint;
  FDefaultImageNameInIconLibrary := 'Zoom\ZoomOut.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetZoomOut.DoExecute;
begin
  ActiveTable.Options.ZoomFactor := ActiveTable.Options.ZoomFactor - 10;
end;

function TdxSpreadSheetZoomOut.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (ActiveTable.Options.ZoomFactor > dxSpreadSheetMinimumZoomFactor);
end;

{ TdxSpreadSheetFreezeFirstColumn }

constructor TdxSpreadSheetFreezeFirstColumn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionFreezeFirstColumnCaption;
  FDefaultHintResString := @sdxSpreadSheetActionFreezeFirstColumnHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\FreezeFirstColumn.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetFreezeFirstColumn.DoExecute;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    ActiveTable.FreezeColumns(ActiveTable.LeftColumn);
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetFreezePanes }

constructor TdxSpreadSheetFreezePanes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionFreezePanesCaption;
  FDefaultHintResString := @sdxSpreadSheetActionFreezePanesHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\FreezePanes.png';
end;

procedure TdxSpreadSheetFreezePanes.DoExecute;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    if (ActiveSelection.FocusedRow <> 0) or (ActiveSelection.FocusedColumn <> 0) then
      ActiveTable.FreezePanes(ActiveSelection.FocusedRow - 1, ActiveSelection.FocusedColumn - 1)
    else
      ActiveTable.FreezePanes(
        cxHalfCoordinate(ActiveTable.TopRow + ActiveTable.BottomRow),
        cxHalfCoordinate(ActiveTable.LeftColumn + ActiveTable.RightColumn))
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetFreezeTopRow }

constructor TdxSpreadSheetFreezeTopRow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionFreezeTopRowCaption;
  FDefaultHintResString := @sdxSpreadSheetActionFreezeTopRowHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\FreezeTopRow.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetFreezeTopRow.DoExecute;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    ActiveTable.FreezeRows(ActiveTable.TopRow);
  finally
    History.EndAction;
  end;
end;

{ TdxSpreadSheetUnfreezePanes }

constructor TdxSpreadSheetUnfreezePanes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUnfreezePanesCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUnfreezePanesHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\UnfreezePanes.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetUnfreezePanes.DoExecute;
begin
  History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
  try
    ActiveTable.UnfreezePanes;
  finally
    History.EndAction;
  end;
end;

function TdxSpreadSheetUnfreezePanes.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ((ActiveTable.FrozenColumn <> -1) or (ActiveTable.FrozenRow <> -1));
end;

{ TdxSpreadSheetChangeFillColor }

constructor TdxSpreadSheetChangeFillColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionChangeFillColorCaption;
  FDefaultHintResString := @sdxSpreadSheetActionChangeFillColorHint;
  FDefaultImageNameInIconLibrary := 'Format\PictureShapeFillColor.png';
end;

procedure TdxSpreadSheetChangeFillColor.DoUpdateState;
begin
  inherited DoUpdateState;
  DoActionValueChanged(ActiveCellStyle.Brush.BackgroundColor);
end;

procedure TdxSpreadSheetChangeFillColor.DoExecute;
begin
  ProcessSelectedCellStyles(
    procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
    begin
      ACellStyle.Brush.BackgroundColor := Value;
    end);
end;

{ TdxSpreadSheetChangeFontColor }

constructor TdxSpreadSheetChangeFontColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionChangeFontColorCaption;
  FDefaultHintResString := @sdxSpreadSheetActionChangeFontColorHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\FontColor.png';
end;

procedure TdxSpreadSheetChangeFontColor.DoUpdateState;
var
  AEdit: TcxCustomRichEdit;
begin
  inherited DoUpdateState;

  if GetEdit(AEdit) then
    DoActionValueChanged(AEdit.SelAttributes.Color)
  else
    DoActionValueChanged(ActiveCellStyle.Font.Color);
end;

procedure TdxSpreadSheetChangeFontColor.DoExecute;
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    AEdit.SelAttributes.Color := Value
  else
    ProcessSelectedCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Color := Value;
      end);
end;

function TdxSpreadSheetChangeFontColor.EnabledWhenEditing: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetChangeFontName }

constructor TdxSpreadSheetChangeFontName.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionChangeFontNameCaption;
  FDefaultHintResString := @sdxSpreadSheetActionChangeFontNameHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetChangeFontName.DoUpdateState;
var
  AEdit: TcxCustomRichEdit;
begin
  inherited DoUpdateState;

  if GetEdit(AEdit) then
    DoActionValueChanged(AEdit.SelAttributes.Name)
  else
    DoActionValueChanged(ActiveCellStyle.Font.Name);
end;

procedure TdxSpreadSheetChangeFontName.DoExecute;
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    AEdit.SelAttributes.Name := Value
  else
    ProcessSelectedCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Name := Value;
      end);
end;

function TdxSpreadSheetChangeFontName.EnabledWhenEditing: Boolean;
begin
  Result := True;
end;

{ TdxSpreadSheetChangeFontSize }

constructor TdxSpreadSheetChangeFontSize.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionChangeFontSizeCaption;
  FDefaultHintResString := @sdxSpreadSheetActionChangeFontSizeHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetChangeFontSize.DoUpdateState;
var
  AEdit: TcxCustomRichEdit;
begin
  inherited DoUpdateState;

  if GetEdit(AEdit) then
    DoActionValueChanged(AEdit.SelAttributes.Size)
  else
    DoActionValueChanged(ActiveCellStyle.Font.Size);
end;

procedure TdxSpreadSheetChangeFontSize.DoExecute;
var
  AEdit: TcxCustomRichEdit;
begin
  if GetEdit(AEdit) then
    AEdit.SelAttributes.Size := Value
  else
    ProcessSelectedCellStyles(
      procedure (ACellStyle: TdxSpreadSheetCellStyle; ARow, AColumn: Integer; const AArea: TRect)
      begin
        ACellStyle.Font.Size := Value;
      end);
end;

function TdxSpreadSheetChangeFontSize.EnabledWhenEditing: Boolean;
begin
  Result := True;
end;


{ TdxSpreadSheetRedo }

constructor TdxSpreadSheetRedo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionRedoCaption;
  FDefaultHintResString := @sdxSpreadSheetActionRedoHint;
  FDefaultImageNameInIconLibrary := 'History\Redo.png';
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetRedo.DoExecute;
begin
  History.Redo;
end;

function TdxSpreadSheetRedo.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (History.RedoActionCount > 0);
end;

{ TdxSpreadSheetUndo }

constructor TdxSpreadSheetUndo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionUndoCaption;
  FDefaultHintResString := @sdxSpreadSheetActionUndoHint;
  FDefaultImageNameInIconLibrary := 'History\Undo.png';
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetUndo.DoExecute;
begin
  History.Undo;
end;

function TdxSpreadSheetUndo.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (History.UndoActionCount > 0);
end;

{ TdxSpreadSheetNewDocument }

constructor TdxSpreadSheetNewDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionNewDocumentCaption;
  FDefaultHintResString := @sdxSpreadSheetActionNewDocumentHint;
  FDefaultImageNameInIconLibrary := 'Actions\New.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetNewDocument.DoExecute;
begin
  Control.BeginUpdate;
  try
    Control.ClearAll;
    Control.AddSheet;
  finally
    Control.EndUpdate;
  end;
end;

{ TdxSpreadSheetOpenDocument }

constructor TdxSpreadSheetOpenDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionOpenDocumentCaption;
  FDefaultHintResString := @sdxSpreadSheetActionOpenDocumentHint;
  FDefaultImageNameInIconLibrary := 'Actions\Open.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetOpenDocument.DoExecute;
var
  AOpenDialog: TOpenDialog;
begin
  AOpenDialog := TOpenDialog.Create(nil);
  try
    AOpenDialog.Filter := dxSpreadSheetFormatsRepository.GetOpenDialogFilter;
    AOpenDialog.FileName := '';
    if AOpenDialog.Execute then
      Control.LoadFromFile(AOpenDialog.FileName);
  finally
    AOpenDialog.Free;
  end;
end;

{ TdxSpreadSheetSaveDocumentAs }

constructor TdxSpreadSheetSaveDocumentAs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionSaveDocumentAsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionSaveDocumentAsHint;
  FDefaultImageNameInIconLibrary := 'Save\SaveAs.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetSaveDocumentAs.DoExecute;
var
  ASaveDialog: TSaveDialog;
begin
  ASaveDialog := TSaveDialog.Create(nil);
  try
    ASaveDialog.DefaultExt := 'xls';
    ASaveDialog.Filter := dxSpreadSheetFormatsRepository.GetSaveDialogFilter;
    ASaveDialog.FileName := '';
    if ASaveDialog.Execute then
      Control.SaveToFile(ASaveDialog.FileName);
  finally
    ASaveDialog.Free;
  end;
end;

{ TdxSpreadSheetFindAndReplace }

constructor TdxSpreadSheetFindAndReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionFindAndReplaceCaption;
  FDefaultHintResString := @sdxSpreadSheetActionFindAndReplaceHint;
  FDefaultImageNameInIconLibrary := 'Find\Find.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetFindAndReplace.DoExecute;
begin
  ShowFindAndReplaceDialog(Control);
end;

{ TdxSpreadSheetShowHyperlinkEditor }

constructor TdxSpreadSheetShowHyperlinkEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionShowHyperlinkEditorCaption;
  FDefaultHintResString := @sdxSpreadSheetActionShowHyperlinkEditorHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\Hyperlink.png';
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetShowHyperlinkEditor.DoExecute;
begin
  ShowHyperlinkEditorDialog(ActiveTable,
    ActiveTable.Hyperlinks.FindItem(ActiveSelection.FocusedRow, ActiveSelection.FocusedColumn));
end;

function TdxSpreadSheetShowHyperlinkEditor.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and ActiveTable.OptionsProtection.ActualAllowEditHyperlinks;
end;

{ TdxSpreadSheetInsertPicture }

constructor TdxSpreadSheetInsertPicture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionInsertPictureCaption;
  FDefaultHintResString := @sdxSpreadSheetActionInsertPictureHint;
  FDefaultImageNameInIconLibrary := 'Content\Image.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetInsertPicture.DoExecute;
var
  AOpenPictureDialog: TOpenPictureDialog;
begin
  ActiveTable.BeginUpdate;
  try
    AOpenPictureDialog := TOpenPictureDialog.Create(nil);
    try
      AOpenPictureDialog.Title := 'Insert Picture';
      if AOpenPictureDialog.Execute then
        DoInsertPictureFromFile(AOpenPictureDialog.FileName);
    finally
      AOpenPictureDialog.Free;
    end;
  finally
    ActiveTable.EndUpdate;
  end;
end;

function TdxSpreadSheetInsertPicture.AddPictureContainer(AWidth, AHeight: Integer): TdxSpreadSheetPictureContainer;
begin
  Result := TdxSpreadSheetPictureContainer(ActiveTable.Containers.Add(TdxSpreadSheetPictureContainer));
  Result.AnchorType := catAbsolute;
  Result.AnchorPoint2.Offset := Point(AWidth, AHeight);
end;

procedure TdxSpreadSheetInsertPicture.DoInsertPictureFromFile(const AFileName: string);
var
  AContainer: TdxSpreadSheetPictureContainer;
  AImage: TdxSmartImage;
begin
  AImage := TdxSmartImage.Create;
  try
    AImage.LoadFromFile(AFileName);
    AContainer := AddPictureContainer(AImage.Width, AImage.Height);
    AContainer.BeginChanging;
    try
      AContainer.Picture.Image := AImage;
      AContainer.Focused := True;
    finally
      AContainer.EndChanging;
    end;
  finally
    AImage.Free;
  end;
end;

{ TdxSpreadSheetProtectSheet }

constructor TdxSpreadSheetProtectSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionProtectSheetCaption;
  FDefaultHintResString := @sdxSpreadSheetActionProtectSheetHint;
  FDefaultImageNameInIconLibrary := 'SpreadSheet\ProtectSheet.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetProtectSheet.DoExecute;
begin
  if Checked then
    ActiveTable.Protect
  else
    ActiveTable.Unprotect;
end;

procedure TdxSpreadSheetProtectSheet.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := ActiveTable.OptionsProtection.Protected;
end;

procedure TdxSpreadSheetProtectSheet.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

{ TdxSpreadSheetProtectWorkbook }

constructor TdxSpreadSheetProtectWorkbook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionProtectWorkbookCaption;
  FDefaultHintResString := @sdxSpreadSheetActionProtectWorkbookHint;
  FDefaultImageNameInIconLibrary := 'SpreadSheet\ProtectWorkbook.png';
  FIsSelectionNeeded := False;
end;

procedure TdxSpreadSheetProtectWorkbook.DoExecute;
begin
  if Checked then
    Control.Protect
  else
    Control.Unprotect;
end;

procedure TdxSpreadSheetProtectWorkbook.DoResetState;
begin
  inherited DoResetState;
  Checked := False;
end;

procedure TdxSpreadSheetProtectWorkbook.DoUpdateState;
begin
  inherited DoUpdateState;
  Checked := Control.OptionsProtection.Protected;
end;

{ TdxSpreadSheetShowPageSetupForm }

constructor TdxSpreadSheetShowPageSetupForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionPageSetupCaption;
  FDefaultHintResString := @sdxSpreadSheetActionPageSetupHint;
  FDefaultImageNameInIconLibrary := 'Setup\PageSetup.png';
end;

function TdxSpreadSheetShowPageSetupForm.GetControlClass: TWinControlClass;
begin
  Result := TdxCustomSpreadSheet;
end;

{ TdxSpreadSheetShowPrintForm }

constructor TdxSpreadSheetShowPrintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionPrintCaption;
  FDefaultHintResString := @sdxSpreadSheetActionPrintHint;
  FDefaultImageNameInIconLibrary := 'Print\PrintDialog.png';
end;

function TdxSpreadSheetShowPrintForm.GetControlClass: TWinControlClass;
begin
  Result := TdxCustomSpreadSheet;
end;

{ TdxSpreadSheetShowPrintPreviewForm }

constructor TdxSpreadSheetShowPrintPreviewForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionPrintPreviewCaption;
  FDefaultHintResString := @sdxSpreadSheetActionPrintPreviewHint;
  FDefaultImageNameInIconLibrary := 'Print\Preview.png';
end;

function TdxSpreadSheetShowPrintPreviewForm.GetControlClass: TWinControlClass;
begin
  Result := TdxCustomSpreadSheet;
end;

{ TdxSpreadSheetCustomPrintingAction }

constructor TdxSpreadSheetCustomPrintingAction.Create(AOwner: TComponent);
begin
  inherited;
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetCustomPrintingAction.DoExecute;
var
  AHandled: Boolean;
begin
  AHandled := False;
  History.BeginAction(TdxSpreadSheetHistoryChangePrintingOptionsAction);
  try
    History.AddCommand(TdxSpreadSheetHistoryChangePrintingOptionsCommand.Create);
    AHandled := DoExecuteCore;
  finally
    History.EndAction(not AHandled);
  end;
end;

function TdxSpreadSheetCustomPrintingAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and not ActiveTable.IsEditing;
end;

function TdxSpreadSheetCustomPrintingAction.GetOptionsPrint: TdxSpreadSheetTableViewOptionsPrint;
begin
  Result := ActiveTable.OptionsPrint;
end;

{ TdxSpreadSheetClearPrintArea }

constructor TdxSpreadSheetClearPrintArea.Create(AOwner: TComponent);
begin
  inherited;
  FIsSelectionNeeded := False;
  FDefaultCaptionResString := @sdxSpreadSheetActionClearPrintAreaCaption;
  FDefaultHintResString := @sdxSpreadSheetActionClearPrintAreaHint;
end;

function TdxSpreadSheetClearPrintArea.DoExecuteCore: Boolean;
begin
  OptionsPrint.Source.Area.Reset;
  Result := True;
end;

function TdxSpreadSheetClearPrintArea.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and OptionsPrint.Source.Area.Assigned;
end;

{ TdxSpreadSheetSetPrintArea }

constructor TdxSpreadSheetSetPrintArea.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionSetPrintAreaCaption;
  FDefaultHintResString := @sdxSpreadSheetActionSetPrintAreaHint;
  FDefaultImageNameInIconLibrary := 'Print\PrintArea.png';
end;

function TdxSpreadSheetSetPrintArea.DoExecuteCore: Boolean;
var
  AArea: TRect;
begin
  AArea := ActiveSelection.Area;
  if dxSpreadSheetIsSingleCellArea(AArea) then
  begin
    if MessageDlg(
      cxGetResourceString(@sdxSetSingleCellAsPrintAreaConfirmation), mtWarning, [mbOK, mbCancel], 0) <> mrOK then
      Exit(False);
  end;
  OptionsPrint.Source.Area.Rect := AArea;
  Result := True;
end;

{ TdxSpreadSheetInsertPageBreak }

constructor TdxSpreadSheetInsertPageBreak.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionInsertPageBreakCaption;
  FDefaultHintResString := @sdxSpreadSheetActionInsertPageBreakHint;
end;

function TdxSpreadSheetInsertPageBreak.DoExecuteCore: Boolean;

  function TryAddPageBreak(AList: TList<Cardinal>; AIndex: Integer): Boolean;
  begin
    Result := AList.IndexOf(AIndex) < 0;
    if Result then
      AList.Add(AIndex);
  end;

begin
  Result := TryAddPageBreak(OptionsPrint.Pagination.ColumnPageBreaks, ActiveSelection.Area.Left);
  Result := TryAddPageBreak(OptionsPrint.Pagination.RowPageBreaks, ActiveSelection.Area.Top) or Result;
end;

{ TdxSpreadSheetRemovePageBreak }

constructor TdxSpreadSheetRemovePageBreak.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionRemovePageBreakCaption;
  FDefaultHintResString := @sdxSpreadSheetActionRemovePageBreakHint;
end;

function TdxSpreadSheetRemovePageBreak.DoExecuteCore: Boolean;
begin
  Result := (OptionsPrint.Pagination.ColumnPageBreaks.Remove(ActiveSelection.Area.Left) >= 0);
  Result := (OptionsPrint.Pagination.RowPageBreaks.Remove(ActiveSelection.Area.Top) >= 0) or Result;
end;

{ TdxSpreadSheetResetAllPageBreaks }

constructor TdxSpreadSheetResetAllPageBreaks.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionResetAllPageBreaksCaption;
  FDefaultHintResString := @sdxSpreadSheetActionResetAllPageBreaksHint;
end;

function TdxSpreadSheetResetAllPageBreaks.DoExecuteCore: Boolean;
begin
  OptionsPrint.Pagination.ColumnPageBreaks.Clear;
  OptionsPrint.Pagination.RowPageBreaks.Clear;
  Result := True;
end;

function TdxSpreadSheetResetAllPageBreaks.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and (
    (OptionsPrint.Pagination.ColumnPageBreaks.Count > 0) or
    (OptionsPrint.Pagination.RowPageBreaks.Count > 0));
end;

{ TdxSpreadSheetCustomPageSetupAction }

constructor TdxSpreadSheetCustomPageSetupAction.Create(AOwner: TComponent);
begin
  inherited;
  FIsContainerSelectionSupported := True;
end;

procedure TdxSpreadSheetCustomPageSetupAction.DoExecute;
begin
  ShowPageSetupDialog(ActiveTable, GetPageIndex);
end;

function TdxSpreadSheetCustomPageSetupAction.GetPageIndex: Integer;
begin
  Result := -1;
end;

function TdxSpreadSheetCustomPageSetupAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and not ActiveTable.IsEditing;
end;

{ TdxSpreadSheetPrintTitles }

constructor TdxSpreadSheetPrintTitles.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionPrintTitlesCaption;
  FDefaultHintResString := @sdxSpreadSheetActionPrintTitlesHint;
  FDefaultImageNameInIconLibrary := 'Rich Edit\Header.png';
end;

function TdxSpreadSheetPrintTitles.GetPageIndex: Integer;
begin
  Result := 3;
end;

{ TdxSpreadSheetSetLandscapePageOrientation }

constructor TdxSpreadSheetSetLandscapePageOrientation.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionSetLandscapePageOrientationCaption;
  FDefaultImageNameInIconLibrary := 'Pages\PageOrientationLandscape.png';
end;

function TdxSpreadSheetSetLandscapePageOrientation.DoExecuteCore: Boolean;
begin
  OptionsPrint.Page.Orientation := oppoLandscape;
  Result := True;
end;

procedure TdxSpreadSheetSetLandscapePageOrientation.DoUpdateState;
begin
  inherited;
  Checked := OptionsPrint.Page.Orientation = oppoLandscape;
end;

{ TdxSpreadSheetSetPortraitPageOrientation }

constructor TdxSpreadSheetSetPortraitPageOrientation.Create(AOwner: TComponent);
begin
  inherited;
  AutoCheck := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionSetPortraitPageOrientationCaption;
  FDefaultImageNameInIconLibrary := 'Pages\PageOrientationPortrait.png';
end;

function TdxSpreadSheetSetPortraitPageOrientation.DoExecuteCore: Boolean;
begin
  OptionsPrint.Page.Orientation := oppoPortrait;
  Result := True;
end;

procedure TdxSpreadSheetSetPortraitPageOrientation.DoUpdateState;
begin
  inherited;
  Checked := OptionsPrint.Page.Orientation <> oppoLandscape;
end;

{ TdxSpreadSheetCustomGalleryAction }

constructor TdxSpreadSheetCustomGalleryAction.Create(AOwner: TComponent);
begin
  inherited;
  dxResourceStringsRepository.AddListener(Self);
end;

destructor TdxSpreadSheetCustomGalleryAction.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  inherited;
end;

function TdxSpreadSheetCustomGalleryAction.FindGalleryGroup(const AName: string): TdxCustomGalleryGroup;
var
  AOwner: TComponent;
begin
  AOwner := Owner;
  while AOwner.Owner is TWinControl do
    AOwner := AOwner.Owner;
  Result := AOwner.FindComponent(AName) as TdxCustomGalleryGroup;
end;

function TdxSpreadSheetCustomGalleryAction.FindGalleryGroupItem(AGroup: TdxCustomGalleryGroup;
  AActionIndex: Variant): TdxCustomGalleryItem;
var
  I: Integer;
begin
  Result := nil;
  if AGroup <> nil then
    for I := 0 to AGroup.ItemCount - 1 do
      if TdxCustomGalleryItemAccess(AGroup.Items[I]).FActionIndex = AActionIndex then
      begin
        Result := AGroup.Items[I];
        Break;
      end;
end;

procedure TdxSpreadSheetCustomGalleryAction.UpdateGalleryContentResourceStrings;
begin
  // do nothing
end;

procedure TdxSpreadSheetCustomGalleryAction.TranslationChanged;
begin
  BeginUpdate;
  try
    UpdateGalleryContentResourceStrings;
  finally
    CancelUpdate;
  end;
end;

{ TdxSpreadSheetPaperSizeGallery }

constructor TdxSpreadSheetPaperSizeGallery.Create(AOwner: TComponent);
begin
  inherited;
  FIsContainerSelectionSupported := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionPaperSizeGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Pages\PaperSize.png';
end;

function TdxSpreadSheetPaperSizeGallery.GetValue: Variant;
begin
  Result := TdxPrintingDefaults.Papers[0].DMPaper;
  if ActiveTable <> nil then
    Result := Max(ActiveTable.OptionsPrint.Page.Paper.SizeID, Result);
end;

procedure TdxSpreadSheetPaperSizeGallery.SetValue(const AValue: Variant);
begin
  ActiveTable.OptionsPrint.Page.Paper.SizeID := AValue;
end;

procedure TdxSpreadSheetPaperSizeGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);

  function GetPaperDescription(const AHeight, AWidth: Integer): string;
  var
    AUnitsConverter: TdxUnitsConverterClass;
    AUnitsName: string;
  begin
    if dxGetDefaultMeasurementUnits = muInches then
    begin
      AUnitsName := cxGetResourceString(@sdxPageSetupDialogUnitsInches);
      AUnitsConverter := TdxInchesUnits;
    end
    else
    begin
      AUnitsName := cxGetResourceString(@sdxPageSetupDialogUnitsMillimeters);
      AUnitsConverter := TdxMillimetersUnits;
    end;
    Result := Format('%f %s x %f %s', [AUnitsConverter.FromLoMetric(AHeight), AUnitsName,
      AUnitsConverter.FromLoMetric(AWidth), AUnitsName]);
  end;

  function GetImageNameByDefaultPaperIndex(const AIndex: Integer): string;
  begin
    case AIndex of
      0:
        Result := 'Pages\PaperKind_Letter.png';
      2:
        Result := 'Pages\PaperKind_Tabloid.png';
      4:
        Result := 'Pages\PaperKind_Legal.png';
      5:
        Result := 'Miscellaneous\EmptySpace.png';
      6:
        Result := 'Pages\PaperKind_Executive.png';
      7:
        Result := 'Pages\PaperKind_A3.png';
      8:
        Result := 'Pages\PaperKind_A4.png';
      10:
        Result := 'Pages\PaperKind_A5.png';
      11:
        Result := 'Miscellaneous\EmptySpace.png';
      12:
        Result := 'Miscellaneous\EmptySpace.png';
    else
      Result := '';
    end;
  end;

const
  CVisibleDefaultPaperIndexes = [0, 2, 4, 5, 6, 7, 8, 10, 11, 12];
var
  APaper: TdxPaper;
  I: Integer;
begin
  for I := 0 to TdxPrintingDefaults.Papers.Count - 1 do
  begin
    APaper := TdxPrintingDefaults.Papers[I];
    if (APaper.ShortName <> '') and (I in CVisibleDefaultPaperIndexes) then
      AInfo.Add(APaper.DMPaper, '', APaper.ShortName,
        GetPaperDescription(APaper.Width, APaper.Height),
        GetImageNameByDefaultPaperIndex(I));
  end;
end;

{ TdxSpreadSheetMorePaperSizes }

constructor TdxSpreadSheetMorePaperSizes.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionMorePaperSizesCaption;
end;

function TdxSpreadSheetMorePaperSizes.GetPageIndex: Integer;
begin
  Result := 0;
end;

{ TdxSpreadSheetPageMarginsGallery }

constructor TdxSpreadSheetPageMarginsGallery.Create(AOwner: TComponent);
begin
  inherited;
  FIsContainerSelectionSupported := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionPageMarginsGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Pages\PageMargins.png';
end;

procedure TdxSpreadSheetPageMarginsGallery.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetPageMarginsGallery then
    FGalleryGroup := TdxSpreadSheetPageMarginsGallery(Source).FGalleryGroup;
end;

procedure TdxSpreadSheetPageMarginsGallery.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroup', ReadGalleryGroup, WriteGalleryGroup, FGalleryGroup <> nil);
end;

procedure TdxSpreadSheetPageMarginsGallery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGalleryGroup) then
    FGalleryGroup := nil;
end;

procedure TdxSpreadSheetPageMarginsGallery.UpdateGalleryContentResourceStrings;

  procedure UpdateGalleryGroupItemResourceStrings(const APredefinedMarginsSet: TPredefinedMarginsSet);
  var
    AItem: TdxCustomGalleryItem;
  begin
    AItem := FindGalleryGroupItem(FGalleryGroup, APredefinedMarginsSet);
    if AItem <> nil then
    begin
      AItem.Caption := GetPredefinedMarginsSetCaption(APredefinedMarginsSet);
      AItem.Description := GetPredefinedMarginsSetDescription(APredefinedMarginsSet);
    end;
  end;

var
  APredefinedMarginsSet: TPredefinedMarginsSet;
begin
  if (Control <> nil) and (FGalleryGroup <> nil) then
    for APredefinedMarginsSet := Low(TPredefinedMarginsSet) to High(TPredefinedMarginsSet) do
      UpdateGalleryGroupItemResourceStrings(APredefinedMarginsSet);
end;

function TdxSpreadSheetPageMarginsGallery.GetValue: Variant;

  function AreSameMargins(AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins; const AMarginsSet: TMarginsSet): Boolean;
  begin
    Result := (AMargins.Top = AMarginsSet[mpTop]) and (AMargins.Bottom = AMarginsSet[mpBottom]) and
      (AMargins.Left = AMarginsSet[mpLeft]) and (AMargins.Right = AMarginsSet[mpRight]) and
      (AMargins.Header = AMarginsSet[mpHeader]) and (AMargins.Footer = AMarginsSet[mpFooter]);
  end;

var
  AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
begin
  Result := Null;
  if ActiveTable <> nil then
  begin
    AMargins := ActiveTable.OptionsPrint.Page.Margins;
    if AreSameMargins(AMargins, CPredefinedNormalMarginsSet) then
      Result := pmsNormal
    else if AreSameMargins(AMargins, CPredefinedWideMarginsSet) then
      Result := pmsWide
    else if AreSameMargins(AMargins, CPredefinedNarrowMarginsSet) then
      Result := pmsNarrow;
  end;
end;

procedure TdxSpreadSheetPageMarginsGallery.SetValue(const AValue: Variant);

  procedure SetMarginsFromMarginsSet(const AMarginsSet: TMarginsSet);
  var
    AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
  begin
    AMargins := ActiveTable.OptionsPrint.Page.Margins;
    AMargins.Top := AMarginsSet[mpTop];
    AMargins.Bottom := AMarginsSet[mpBottom];
    AMargins.Left := AMarginsSet[mpLeft];
    AMargins.Right := AMarginsSet[mpRight];
    AMargins.Header := AMarginsSet[mpHeader];
    AMargins.Footer := AMarginsSet[mpFooter];
  end;

begin
  case TPredefinedMarginsSet(AValue) of
    pmsNormal:
      SetMarginsFromMarginsSet(CPredefinedNormalMarginsSet);
    pmsWide:
      SetMarginsFromMarginsSet(CPredefinedWideMarginsSet);
    pmsNarrow:
      SetMarginsFromMarginsSet(CPredefinedNarrowMarginsSet);
  end;
end;

procedure TdxSpreadSheetPageMarginsGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
    FGalleryGroup := TdxCustomGalleryGroups(AInfo.GetGroups).Add
  else
    FGalleryGroup := nil;

  AInfo.Add(pmsNormal, '', GetPredefinedMarginsSetCaption(pmsNormal),
    GetPredefinedMarginsSetDescription(pmsNormal), 'Pages\PageMarginsNormal.png');
  AInfo.Add(pmsWide, '', GetPredefinedMarginsSetCaption(pmsWide),
    GetPredefinedMarginsSetDescription(pmsWide), 'Pages\PageMarginsWide.png');
  AInfo.Add(pmsNarrow, '', GetPredefinedMarginsSetCaption(pmsNarrow),
    GetPredefinedMarginsSetDescription(pmsNarrow), 'Pages\PageMarginsNarrow.png');
end;

function TdxSpreadSheetPageMarginsGallery.GetPredefinedMarginsSet(
  const APredefinedMarginsSet: TPredefinedMarginsSet): TMarginsSet;
begin
  case APredefinedMarginsSet of
    pmsNormal:
      Result := CPredefinedNormalMarginsSet;
    pmsWide:
      Result := CPredefinedWideMarginsSet;
  else //pmsNarrow:
    Result := CPredefinedNarrowMarginsSet;
  end;
end;

function TdxSpreadSheetPageMarginsGallery.GetPredefinedMarginsSetCaption(
  const APredefinedMarginsSet: TPredefinedMarginsSet): string;
begin
  case APredefinedMarginsSet of
    pmsNormal:
      Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryNormalMarginsCaption);
    pmsWide:
      Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryWideMarginsCaption);
  else //pmsNarrow:
    Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryNarrowMarginsCaption);
  end;
end;

function TdxSpreadSheetPageMarginsGallery.GetPredefinedMarginsSetDescription(
  const APredefinedMarginsSet: TPredefinedMarginsSet): string;

  function GetMarginsPartCaption(const APart: TMarginsPart): string;
  begin
    case APart of
      mpTop:
        Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryTopPartCaption);
      mpBottom:
        Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryBottomPartCaption);
      mpLeft:
        Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryLeftPartCaption);
      mpRight:
        Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryRightPartCaption);
      mpHeader:
        Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryHeaderPartCaption);
      mpFooter:
        Result := cxGetResourceString(@sdxSpreadSheetActionPageMarginsGalleryFooterPartCaption);
    end;
  end;

var
  AMarginsSet: TMarginsSet;
  AUnitsName: string;
begin
  AMarginsSet := GetPredefinedMarginsSet(APredefinedMarginsSet);
  AUnitsName := cxGetResourceString(@sdxPageSetupDialogUnitsInches);
  Result :=
    Format('%s:'#9'%2.2f %s    %s:'#9'%2.2f %s', [GetMarginsPartCaption(mpTop), AMarginsSet[mpTop], AUnitsName,
      GetMarginsPartCaption(mpBottom), AMarginsSet[mpBottom], AUnitsName]) + #13#10 +
    Format('%s:'#9'%2.2f %s    %s:'#9'%2.2f %s', [GetMarginsPartCaption(mpLeft), AMarginsSet[mpLeft], AUnitsName,
      GetMarginsPartCaption(mpRight), AMarginsSet[mpRight], AUnitsName]) + #13#10 +
    Format('%s:'#9'%2.2f %s    %s:'#9'%2.2f %s', [GetMarginsPartCaption(mpHeader), AMarginsSet[mpHeader],
      AUnitsName, GetMarginsPartCaption(mpFooter), AMarginsSet[mpFooter], AUnitsName]);
end;

procedure TdxSpreadSheetPageMarginsGallery.ReadGalleryGroup(Reader: TReader);
begin
  FGalleryGroup := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetPageMarginsGallery.WriteGalleryGroup(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroup.Name);
end;

{ TdxSpreadSheetMorePageMargins }

constructor TdxSpreadSheetMorePageMargins.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionMorePageMarginsCaption;
end;

function TdxSpreadSheetMorePageMargins.GetPageIndex: Integer;
begin
  Result := 1;
end;

{ TdxSpreadSheetAutoSumGallery }

constructor TdxSpreadSheetAutoSumGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionAutoSumGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Function Library\AutoSum.png';
end;

procedure TdxSpreadSheetAutoSumGallery.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetAutoSumGallery then
    FGalleryGroup := TdxSpreadSheetAutoSumGallery(Source).FGalleryGroup;
end;

procedure TdxSpreadSheetAutoSumGallery.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroup', ReadGalleryGroup, WriteGalleryGroup, FGalleryGroup <> nil);
end;

procedure TdxSpreadSheetAutoSumGallery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGalleryGroup) then
    FGalleryGroup := nil;
end;

procedure TdxSpreadSheetAutoSumGallery.UpdateGalleryContentResourceStrings;

  procedure UpdateGalleryGroupItemResourceStrings(const AFormula: TFormula);
  var
    AItem: TdxCustomGalleryItem;
  begin
    AItem := FindGalleryGroupItem(FGalleryGroup, AFormula);
    if AItem <> nil then
    begin
      AItem.Caption := GetFormulaCaption(AFormula);
      AItem.Description := GetFormulaDescription(AFormula);
    end;
  end;

var
  AFormula: TFormula;
begin
  if (Control <> nil) and (FGalleryGroup <> nil) then
    for AFormula := Low(TFormula) to High(TFormula) do
      UpdateGalleryGroupItemResourceStrings(AFormula);
end;

function TdxSpreadSheetAutoSumGallery.GetValue: Variant;
begin
  Result := Null;
end;

procedure TdxSpreadSheetAutoSumGallery.SetValue(const AValue: Variant);

  function GetFormulaAsText(const AFormula: TFormula): string;
  begin
    case AFormula of
      fSum:
        Result := cxGetResourceString(@sfnSum);
      fAverage:
        Result := cxGetResourceString(@sfnAverage);
      fCountNumbers:
        Result := cxGetResourceString(@sfnCount);
      fMax:
        Result := cxGetResourceString(@sfnMax);
      fMin:
        Result := cxGetResourceString(@sfnMin);
    else
      Result := '';
    end;
  end;

var
  AFormula: string;
begin
  History.BeginAction(TdxSpreadSheetHistoryAction);
  try
    AFormula := GetFormulaAsText(AValue);
    if AFormula <> '' then
      TdxModifyFormulasHelper.ApplyToSelection(AFormula, ActiveTable);
  finally
    History.EndAction;
  end;
end;

procedure TdxSpreadSheetAutoSumGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);

  function GetImageFileName(const AFormula: TFormula): string;
  begin
    case AFormula of
      fSum:
        Result := 'Function Library\AutoSum.png';
      fAverage, fCountNumbers, fMax, fMin:
        Result := 'Miscellaneous\EmptySpace.png';
    else
      Result := '';
    end;
  end;

var
  AFormula: TFormula;
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
    FGalleryGroup := TdxCustomGalleryGroups(AInfo.GetGroups).Add
  else
    FGalleryGroup := nil;

  for AFormula := Low(TFormula) to High(TFormula) do
    AInfo.Add(AFormula, '', GetFormulaCaption(AFormula), GetFormulaDescription(AFormula), GetImageFileName(AFormula));
end;

function TdxSpreadSheetAutoSumGallery.GetFormulaCaption(const AFormula: TFormula): string;
begin
  case AFormula of
    fSum:
      Result := cxGetResourceString(@sdxSpreadSheetActionAutoSumGallerySumCaption);
    fAverage:
      Result := cxGetResourceString(@sdxSpreadSheetActionAutoSumGalleryAverageCaption);
    fCountNumbers:
      Result := cxGetResourceString(@sdxSpreadSheetActionAutoSumGalleryCountNumbersCaption);
    fMax:
      Result := cxGetResourceString(@sdxSpreadSheetActionAutoSumGalleryMaxCaption);
    fMin:
      Result := cxGetResourceString(@sdxSpreadSheetActionAutoSumGalleryMinCaption);
  else
    Result := '';
  end;
end;

function TdxSpreadSheetAutoSumGallery.GetFormulaDescription(const AFormula: TFormula): string;
begin
  case AFormula of
    fSum:
      Result := cxGetResourceString(@sfnSumDescription);
    fAverage:
      Result := cxGetResourceString(@sfnAverageDescription);
    fCountNumbers:
      Result := cxGetResourceString(@sfnCountDescription);
    fMax:
      Result := cxGetResourceString(@sfnMaxDescription);
    fMin:
      Result := cxGetResourceString(@sfnMinDescription);
  else
    Result := '';
  end;
end;

procedure TdxSpreadSheetAutoSumGallery.ReadGalleryGroup(Reader: TReader);
begin
  FGalleryGroup := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetAutoSumGallery.WriteGalleryGroup(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroup.Name);
end;

{ TdxSpreadSheetCustomFormulasGalleryAction }

constructor TdxSpreadSheetCustomFormulasGalleryAction.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := dxSpreadSheetFunctionTypeNameAsResString(GetTypeID);
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetCustomFormulasGalleryAction then
    FGalleryGroup := TdxSpreadSheetCustomFormulasGalleryAction(Source).FGalleryGroup;
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroup', ReadGalleryGroup, WriteGalleryGroup, FGalleryGroup <> nil);
end;

function TdxSpreadSheetCustomFormulasGalleryAction.GetDefaultCaption: string;
begin
  Result := StringReplace(cxGetResourceString(FDefaultCaptionResString), '&', '&&', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGalleryGroup) then
    FGalleryGroup := nil;
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.UpdateGalleryContentResourceStrings;

  procedure UpdateGalleryGroupItemResourceStrings(AFunctionInfo: TdxSpreadSheetFunctionInfo);
  var
    AItem: TdxCustomGalleryItem;
  begin
    AItem := FindGalleryGroupItem(FGalleryGroup, LoadResString(AFunctionInfo.NamePtr));
    if AItem <> nil then
    begin
      AItem.Caption := cxGetResourceString(AFunctionInfo.NamePtr);
      AItem.Description := cxGetResourceString(AFunctionInfo.DescriptionPtr);
    end;
  end;

var
  AFunctionInfo: TdxSpreadSheetFunctionInfo;
  I: Integer;
begin
  if (Control <> nil) and (FGalleryGroup <> nil) then
    for I := 0 to dxSpreadSheetFunctionsRepository.Count - 1 do
    begin
      AFunctionInfo := dxSpreadSheetFunctionsRepository.Items[I];
      if (AFunctionInfo.TypeID = GetTypeID) and Assigned(AFunctionInfo.Proc) then
        UpdateGalleryGroupItemResourceStrings(AFunctionInfo);
    end;
end;

function TdxSpreadSheetCustomFormulasGalleryAction.GetValue: Variant;
begin
  Result := Null;
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.SetValue(const AValue: Variant);
var
  AEdit: TcxCustomEdit;
  AEditingController: TEditingControllerAccess;
begin
  AEditingController := TEditingControllerAccess(TdxSpreadSheetTableViewAccess(ActiveTable).EditingController);
  if not TdxSpreadSheetTableViewEditingController.GetActualInplaceEdit(
    ActiveTable, TdxSpreadSheetCustomInplaceEdit(AEdit)) then
  begin
    ActiveTable.Controller.FocusedContainer := nil;
    ActiveTable.ShowEdit;
    AEdit := AEditingController.Edit;
  end;
  if AEdit <> nil then
  begin
    {$WARNINGS OFF}
    TdxModifyFormulasHelper.Insert(cxGetResourceString(AValue), AEdit);
    {$WARNINGS ON}
    AEditingController.MultilineEditTextChanged;
  end;
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);
var
  AFunctionInfo: TdxSpreadSheetFunctionInfo;
  I: Integer;
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
    FGalleryGroup := TdxCustomGalleryGroups(AInfo.GetGroups).Add
  else
    FGalleryGroup := nil;

  for I := 0 to dxSpreadSheetFunctionsRepository.Count - 1 do
  begin
    AFunctionInfo := dxSpreadSheetFunctionsRepository.Items[I];
    if (AFunctionInfo.TypeID = GetTypeID) and Assigned(AFunctionInfo.Proc) then
      AInfo.Add(LoadResString(AFunctionInfo.NamePtr), '',
        cxGetResourceString(AFunctionInfo.NamePtr),
        cxGetResourceString(AFunctionInfo.DescriptionPtr), '');
  end;
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.ReadGalleryGroup(Reader: TReader);
begin
  FGalleryGroup := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetCustomFormulasGalleryAction.WriteGalleryGroup(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroup.Name);
end;

{ TdxSpreadSheetFinancialFormulasGallery }

constructor TdxSpreadSheetFinancialFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Financial.png';
end;

function TdxSpreadSheetFinancialFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftFinancial;
end;

{ TdxSpreadSheetLogicalFormulasGallery }

constructor TdxSpreadSheetLogicalFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Logical.png';
end;

function TdxSpreadSheetLogicalFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftLogical;
end;

{ TdxSpreadSheetTextFormulasGallery }

constructor TdxSpreadSheetTextFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Text.png';
end;

function TdxSpreadSheetTextFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftText;
end;

{ TdxSpreadSheetDateAndTimeFormulasGallery }

constructor TdxSpreadSheetDateAndTimeFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Date&Time.png';
end;

function TdxSpreadSheetDateAndTimeFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftDateTime;
end;

{ TdxSpreadSheetLookupAndReferenceFormulasGallery }

constructor TdxSpreadSheetLookupAndReferenceFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Lookup&Reference.png';
end;

function TdxSpreadSheetLookupAndReferenceFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftLookupAndReference;
end;

{ TdxSpreadSheetMathAndTrigFormulasGallery }

constructor TdxSpreadSheetMathAndTrigFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Math&Trig.png';
end;

function TdxSpreadSheetMathAndTrigFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftMath;
end;

{ TdxSpreadSheetStatisticalFormulasGallery }

constructor TdxSpreadSheetStatisticalFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Statistical.png';
end;

function TdxSpreadSheetStatisticalFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftStatistical;
end;

{ TdxSpreadSheetInformationFormulasGallery }

constructor TdxSpreadSheetInformationFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Information.png';
end;

function TdxSpreadSheetInformationFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftInformation;
end;

{ TdxSpreadSheetCompatibilityFormulasGallery }

constructor TdxSpreadSheetCompatibilityFormulasGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultImageNameInIconLibrary := 'Function Library\Compatibility.png';
end;

function TdxSpreadSheetCompatibilityFormulasGallery.GetTypeID: TdxSpreadSheetFunctionType;
begin
  Result := ftCompatibility;
end;

{ TdxSpreadSheetPageOrientationGallery }

constructor TdxSpreadSheetPageOrientationGallery.Create(AOwner: TComponent);
begin
  inherited;
  FIsContainerSelectionSupported := True;
  FDefaultCaptionResString := @sdxSpreadSheetActionPageOrientationGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Pages\PageOrientation.png';
end;

procedure TdxSpreadSheetPageOrientationGallery.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetPageOrientationGallery then
    FGalleryGroup := TdxSpreadSheetPageOrientationGallery(Source).FGalleryGroup;
end;

procedure TdxSpreadSheetPageOrientationGallery.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroup', ReadGalleryGroup, WriteGalleryGroup, FGalleryGroup <> nil);
end;

procedure TdxSpreadSheetPageOrientationGallery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGalleryGroup) then
    FGalleryGroup := nil;
end;

procedure TdxSpreadSheetPageOrientationGallery.UpdateGalleryContentResourceStrings;
var
  AItem: TdxCustomGalleryItem;
begin
  if (Control <> nil) and (FGalleryGroup <> nil) then
  begin
    AItem := FindGalleryGroupItem(FGalleryGroup, oppoPortrait);
    if AItem <> nil then
      AItem.Caption := GetPageOrientationCaption(oppoPortrait);

    AItem := FindGalleryGroupItem(FGalleryGroup, oppoLandscape);
    if AItem <> nil then
      AItem.Caption := GetPageOrientationCaption(oppoLandscape);
  end;
end;

function TdxSpreadSheetPageOrientationGallery.GetValue: Variant;
begin
  Result := oppoDefault;
  if ActiveTable <> nil then
    Result := ActiveTable.OptionsPrint.Page.Orientation;
  if Result = oppoDefault then
    Result := oppoPortrait;
end;

procedure TdxSpreadSheetPageOrientationGallery.SetValue(const AValue: Variant);
begin
  ActiveTable.OptionsPrint.Page.Orientation := AValue;
end;

procedure TdxSpreadSheetPageOrientationGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
  begin
    FGalleryGroup := TdxCustomGalleryGroups(AInfo.GetGroups).Add
  end
  else
    FGalleryGroup := nil;

  AInfo.Add(oppoPortrait, '', GetPageOrientationCaption(oppoPortrait), '', 'Pages\PageOrientationPortrait.png');
  AInfo.Add(oppoLandscape, '', GetPageOrientationCaption(oppoLandscape), '', 'Pages\PageOrientationLandscape.png');
end;

function TdxSpreadSheetPageOrientationGallery.GetPageOrientationCaption(
  AOrientation: TdxSpreadSheetTableViewOptionsPrintPageOrientation): string;
begin
  case AOrientation of
    oppoPortrait:
      Result := cxGetResourceString(@sdxSpreadSheetActionSetPortraitPageOrientationCaption);
    oppoLandscape:
      Result := cxGetResourceString(@sdxSpreadSheetActionSetLandscapePageOrientationCaption);
  else // oppoDefault
    Result := '';
  end;
end;

procedure TdxSpreadSheetPageOrientationGallery.ReadGalleryGroup(Reader: TReader);
begin
  FGalleryGroup := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetPageOrientationGallery.WriteGalleryGroup(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroup.Name);
end;

end.
