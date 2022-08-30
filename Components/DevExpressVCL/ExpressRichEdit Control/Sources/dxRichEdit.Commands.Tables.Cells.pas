{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Commands.Tables.Cells;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Graphics,
  dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.View.Core;

type

  { TdxMergeTableElementCommandBase }

  TdxMergeTableElementCommandBase = class abstract(TdxRichEditSelectionCommand)
  protected
    FSelectedCellsCollection: TdxSelectedCellsCollection;
    function GetTryToKeepCaretX: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure UpdateUIStateEnabled(const AState: IdxCommandUIState);
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function PerformChangeSelection: Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    procedure ChangeSelection(ACellToSelect: TdxTableCell); reintroduce; overload;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure PerformModifyModel; override;
  end;

  { TdxMergeTableElementMenuCommand }

  TdxMergeTableElementMenuCommand = class(TdxMergeTableElementCommandBase)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
  end;

  { TdxMergeTableCellsCommand }

  TdxMergeTableCellsCommand = class(TdxMergeTableElementCommandBase)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
  end;

  { TdxSplitTableCellsCommand }

  TdxSplitTableCellsCommand = class(TdxRichEditSelectionCommand)
  strict private
    FCellsParameters: TdxSplitTableCellsParameters;
    FNewPosition: TdxDocumentLogPosition;
  protected
    function GetTryToKeepCaretX: Boolean; override;
    function GetExtendSelection: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function CalculateNewPosition(AFirstCell: TdxTableCell): TdxDocumentLogPosition;
    procedure SplitTableCellsHorizontally(ASelectedCells: TdxSelectedCellsCollection);
    procedure SplitTableCellsVertically(ASelectedCells: TdxSelectedCellsCollection);
    function GetColumnsCountForSplitVertically(AInterval: TdxSelectedCellsIntervalInRow): Integer;
    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure PerformModifyModel; override;

    property CellsParameters: TdxSplitTableCellsParameters read FCellsParameters write FCellsParameters;
  end;

  { TdxSelectedTableCellPositionInfo }

  TdxSelectedTableCellPositionInfo = record
  private type
    TdxMask = (
      FirstSelectedRow,
      LastSelectedRow,
      FirstSelectedColumn,
      LastSelectedColumn
    );
    TdxMasks = set of TdxMask;
  private
    FMask: TdxMasks;
    FCell: TdxTableCell;
    function GetItem(const Index: TdxMask): Boolean;
    procedure SetItem(const Index: TdxMask; const Value: Boolean);
  public
    constructor Create(ACell: TdxTableCell);
    procedure CopyFrom(const ASource: TdxSelectedTableCellPositionInfo);

    property FirstSelectedRow: Boolean index TdxMask.FirstSelectedRow read GetItem write SetItem;
    property LastSelectedRow: Boolean index TdxMask.LastSelectedRow read GetItem write SetItem;
    property FirstSelectedColumn: Boolean index TdxMask.FirstSelectedColumn read GetItem write SetItem;
    property LastSelectedColumn: Boolean index TdxMask.LastSelectedColumn read GetItem write SetItem;
    property Cell: TdxTableCell read FCell write FCell;
  end;

  { TdxTableCellActualBorders }

  TdxTableCellActualBorders = class(TcxIUnknownObject, IdxTableCellBorders)
  private
    FCell: TdxTableCell;
    FTop: TdxBorderBase;
    FBottom: TdxBorderBase;
    FLeft: TdxBorderBase;
    FRight: TdxBorderBase;
    function GetTopBorder: TdxBorderBase;
    function GetBottomBorder: TdxBorderBase;
    function GetLeftBorder: TdxBorderBase;
    function GetRightBorder: TdxBorderBase;
  public
    constructor Create(ACell: TdxTableCell);

    property TopBorder: TdxBorderBase read GetTopBorder;
    property BottomBorder: TdxBorderBase read GetBottomBorder;
    property LeftBorder: TdxBorderBase read GetLeftBorder;
    property RightBorder: TdxBorderBase read GetRightBorder;
  end;

  { TdxChangeTableCellsCommandBase }

  TdxChangeTableCellsCommandBase = class abstract(TdxRichEditSelectionCommand)
  private
    FExecuteState: IdxCommandUIState;
    FCells: TdxTableCellList;
    FIsSelectedCellsSquare: Boolean;
    procedure CheckInsideTableBorders; overload;
    procedure CheckInsideTableBorder(ATable: TdxTable; AIsCheckingVerticalBorder: Boolean; AInsideBorder: TdxBorderBase; AStyleBorderInfo: TdxBorderInfo); overload;
    procedure SetInsideTableBorder(AInsideBorder: TdxBorderBase; AStyle: TdxBorderLineStyle; AWidth: Integer; AColor: TdxAlphaColor);
  protected
    function GetExtendSelection: Boolean; override;
    function GetTreatStartPositionAsCurrent: Boolean; override;
    function GetTryToKeepCaretX: Boolean; override;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    function CanChangePosition(const APos: TdxDocumentModelPosition): Boolean; override;
    function ChangePosition(const APos: TdxDocumentModelPosition): Integer; override;
    procedure CollectCells(ATarget: TdxTableCellList); overload; virtual;
    procedure CollectCells(ACellsInRow: TdxSelectedCellsIntervalInRow; var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList); overload; virtual;
    procedure CollectCell(ACell: TdxTableCell; var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList); overload; virtual;
    procedure CollectCellsCore(ACells: TdxSelectedCellsCollection; ATarget: TdxTableCellList); virtual;
    function PerformChangeSelection: Boolean; override;
    procedure PerformModifyModelCore(const AState: IdxCommandUIState); virtual;
    procedure ModifyCells(ACells: TdxTableCellList; const AState: IdxCommandUIState); virtual;
    procedure ModifyCell(ACell: TdxTableCell); virtual; abstract;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); overload; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState; const ASelectedCellsCollection: TdxSelectedTableStructureBase); reintroduce; overload; virtual;

    property IsSelectedCellsSquare: Boolean read FIsSelectedCellsSquare;
  public
    destructor Destroy; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure PerformModifyModel; override;
  end;

  { TdxToggleTableCellsBordersCommandBase }

  TdxToggleTableCellsBordersCommandBaseClass = class of TdxToggleTableCellsBordersCommandBase;

  TdxToggleTableCellsBordersCommandBase = class abstract(TdxChangeTableCellsCommandBase)
  private
    FNewBorder: TdxBorderInfo;
    FBorders: TdxBorderBaseList;
    FActualBorders: TdxBorderBaseList;
  protected
    function GetActualNewBorder: TdxBorderInfo;
    function AreTableBordersAffected: Boolean;
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; virtual;
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList); virtual; abstract;
    procedure CollectCells(ATarget: TdxTableCellList); overload; override;
    procedure CollectCells(ACellsInRow: TdxSelectedCellsIntervalInRow; var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList); overload; override;
    procedure CollectCell(ATableCell: TdxTableCell; var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList); override;
    procedure CollectCellsCore(ACells: TdxSelectedCellsCollection; ATarget: TdxTableCellList); override;
    procedure CollectCellsCoreWithMultipleItems(ASelectedCells: TdxSelectedCellsCollection; ATarget: TdxTableCellList);
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); virtual;
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); virtual;
    procedure CollectLeftNeighbourBordersInMergedCells(ACell: TdxTableCell; ABorders: TdxBorderBaseList);
    procedure CollectNeighbourRowCells(ARow: TdxTableRow; ACellsInRow: TdxSelectedCellsIntervalInRow; ATarget: TdxTableCellList);
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); virtual;
    procedure CollectRightNeighbourBordersInMergedCells(aCell: TdxTableCell; ABorders: TdxBorderBaseList);
    procedure CollectTableBorders(ATableBorders: TdxTableBorders; ABorders: TdxBorderBaseList); virtual; abstract;
    procedure CollectTopAndBottomNeighbourBorders(ASelectedCells: TdxSelectedCellsCollection; ATarget: TdxTableCellList);
    procedure CollectTopAndBottomNeighbourBordersCore(ACells: TdxSelectedCellsCollection; ATarget: TdxTableCellList; AFirst, ALast: TdxSelectedCellsIntervalInRow);
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); virtual;
    procedure ModifyBorder(ABorder: TdxBorderBase); virtual;
    procedure ModifyCells(ACells: TdxTableCellList; const AState: IdxCommandUIState); override;
    procedure ModifyCell(ACell: TdxTableCell); override;
    procedure ResetBorder(ABorder: TdxBorderBase); virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    destructor Destroy; override;
    property NewBorder: TdxBorderInfo read FNewBorder write FNewBorder;
  end;

  { TdxToggleTableCellsBottomBorderCommand }

  TdxToggleTableCellsBottomBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders; ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsTopBorderCommand }

  TdxToggleTableCellsTopBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders;
      ABorders: TdxBorderBaseList); override;
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsInsideHorizontalBorderCommand }

  TdxToggleTableCellsInsideHorizontalBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders;
      ABorders: TdxBorderBaseList); override;
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsLeftBorderCommand }

  TdxToggleTableCellsLeftBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders;
      ABorders: TdxBorderBaseList); override;
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsRightBorderCommand }

  TdxToggleTableCellsRightBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders;
      ABorders: TdxBorderBaseList); override;
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsInsideVerticalBorderCommand }

  TdxToggleTableCellsInsideVerticalBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders;
      ABorders: TdxBorderBaseList); override;
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
      ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsAllBordersCommand }

  TdxToggleTableCellsAllBordersCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders; ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxResetTableCellsBordersCommand }

  TdxResetTableCellsBordersCommand = class(TdxToggleTableCellsAllBordersCommand)
  protected
    function CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsOutsideBorderCommand }

  TdxToggleTableCellsOutsideBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders; ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsInsideBorderCommand }

  TdxToggleTableCellsInsideBorderCommand = class(TdxToggleTableCellsBordersCommandBase)
  protected
    procedure CollectBorders(const ACellBorders: IdxTableCellBorders; ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList); override;
    procedure CollectTableBorders(ATableBorders: TdxTableBorders; ABorders: TdxBorderBaseList); override;
    procedure CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
    procedure CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
    procedure CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
    procedure CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsContentAlignmentCommandBase }

  TdxToggleTableCellsContentAlignmentCommandBase = class abstract(TdxChangeTableCellsCommandBase)
  strict private
    FCellsContentIntervals: TdxSelectionItems;
  protected
    function GetParagraphAlignment: TdxParagraphAlignment; virtual; abstract;
    function GetCellVerticalAlignment: TdxVerticalAlignment; virtual; abstract;
    procedure CollectCellsCore(ACells: TdxSelectedCellsCollection; ATarget: TdxTableCellList); override;
    procedure CollectCell(ACell: TdxTableCell; var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList); override;
    procedure PerformModifyModelCore(const AState: IdxCommandUIState); override;
    procedure ModifyCell(ATableCell: TdxTableCell); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState;
      const ASelectedCellsCollection: TdxSelectedTableStructureBase); override;

    property CellsContentIntervals: TdxSelectionItems read FCellsContentIntervals;
    property ParagraphAlignment: TdxParagraphAlignment read GetParagraphAlignment;
    property CellVerticalAlignment: TdxVerticalAlignment read GetCellVerticalAlignment;
  public
    destructor Destroy; override;
  end;

  { TdxToggleTableCellsParagraphAlignmentCommand }

  TdxToggleTableCellsParagraphAlignmentCommand = class(TdxToggleChangeParagraphFormattingCommandBase<TdxParagraphAlignment>)
  strict private
    FOwnerCommand: TdxToggleTableCellsContentAlignmentCommandBase;
  protected
    function GetSelectionItems: TdxSelectionItems; override;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>; override;
    function IsCheckedValue(AValue: TdxParagraphAlignment): Boolean; override;
  public
    constructor Create(const AControl: IdxRichEditControl; AOwnerCommand: TdxToggleTableCellsContentAlignmentCommandBase); reintroduce;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxChangeTableCellsContentAlignmentPlaceholderCommand }

  TdxChangeTableCellsContentAlignmentPlaceholderCommand = class(TdxChangeTableCellsCommandBase)
  protected
    procedure ModifyCell(ATableCell: TdxTableCell); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;

    procedure ForceExecute(const AState: IdxCommandUIState); override;
    function CreateDefaultCommandUIState: IdxCommandUIState; override;
  end;

  { TdxToggleTableCellsTopLeftAlignmentCommand }

  TdxToggleTableCellsTopLeftAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsTopCenterAlignmentCommand  }

  TdxToggleTableCellsTopCenterAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsTopRightAlignmentCommand }

  TdxToggleTableCellsTopRightAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsMiddleLeftAlignmentCommand }

  TdxToggleTableCellsMiddleLeftAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsMiddleCenterAlignmentCommand }

  TdxToggleTableCellsMiddleCenterAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsMiddleRightAlignmentCommand }

  TdxToggleTableCellsMiddleRightAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsBottomLeftAlignmentCommand }

  TdxToggleTableCellsBottomLeftAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsBottomCenterAlignmentCommand }

  TdxToggleTableCellsBottomCenterAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleTableCellsBottomRightAlignmentCommand }

  TdxToggleTableCellsBottomRightAlignmentCommand = class(TdxToggleTableCellsContentAlignmentCommandBase)
  protected
    function GetCellVerticalAlignment: TdxVerticalAlignment; override;
    function GetParagraphAlignment: TdxParagraphAlignment; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Contnrs, Math, dxCore,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Commands.Tables;

type
  TdxSelectionAccess = class(TdxSelection);

{$REGION 'TdxMergeTableElementCommandBase'}

{ TdxMergeTableElementCommandBase }

class function TdxMergeTableElementCommandBase.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMergeTableCellsDescription);
end;

class function TdxMergeTableElementCommandBase.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandMergeTableCellsMenuCaption);
end;

function TdxMergeTableElementCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxMergeTableElementCommandBase.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxMergeTableElementCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxMergeTableElementCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxMergeTableElementCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  UpdateUIStateEnabled(AState);
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

procedure TdxMergeTableElementCommandBase.UpdateUIStateEnabled(const AState: IdxCommandUIState);
var
  ACells: TdxSelectedCellsCollection;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    AState.Enabled := False
  else
  begin
    ACells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
    AState.Enabled := ACells.IsSquare and ((ACells.NormalizedFirst.NormalizedLength > 0) or (ACells.RowsCount > 1)) and
      TdxSelectionAccess(DocumentModel.Selection).IsValidSelectedCells;
  end;
end;

procedure TdxMergeTableElementCommandBase.PerformModifyModel;
begin
  if not DocumentModel.Selection.SelectedCells.IsNotEmpty then
    Exit;
  FSelectedCellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);

  ActivePieceTable.MergeCells(FSelectedCellsCollection);
  FSelectedCellsCollection := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);

end;

function TdxMergeTableElementCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxMergeTableElementCommandBase.PerformChangeSelection: Boolean;
begin
  if FSelectedCellsCollection.NormalizedFirst <> nil then
    ChangeSelection(FSelectedCellsCollection.NormalizedFirst.NormalizedStartCell)
  else
    DocumentModel.Selection.ClearSelectionInTable;
  Result := True;
end;

procedure TdxMergeTableElementCommandBase.ChangeSelection(ACellToSelect: TdxTableCell);
var
  ACommand: TdxSelectTableCellCommand;
begin
  ACommand := TdxSelectTableCellCommand.Create(RichEditControl);
  try
    ACommand.Cell := ACellToSelect;
    ACommand.ChangeSelection(DocumentModel.Selection);
  finally
    ACommand.Free;
  end;
end;

function TdxMergeTableElementCommandBase.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := APos.LogPosition;
end;

{$ENDREGION}

{$REGION 'TdxMergeTableElementMenuCommand'}

{ TdxMergeTableElementMenuCommand }

class function TdxMergeTableElementMenuCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.MergeTableElement;
end;

class function TdxMergeTableElementMenuCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.MergeCells;
end;

procedure TdxMergeTableElementMenuCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{$ENDREGION}

{$REGION 'TdxMergeTableCellsCommand'}

{ TdxMergeTableCellsCommand }

class function TdxMergeTableCellsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.MergeTableCells;
end;

class function TdxMergeTableCellsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.MergeCells;
end;

procedure TdxMergeTableCellsCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandsRestriction(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
end;

{$ENDREGION}

{ TdxSplitTableCellsCommand }

class function TdxSplitTableCellsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsMenuCaption);
end;

class function TdxSplitTableCellsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCellsDescription);
end;

function TdxSplitTableCellsCommand.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxSplitTableCellsCommand.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxSplitTableCellsCommand.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

function TdxSplitTableCellsCommand.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

procedure TdxSplitTableCellsCommand.ForceExecute(const AState: IdxCommandUIState);
var
  AValueState: TdxDefaultValueBasedCommandUIState<TdxSplitTableCellsParameters>;
begin
  AValueState := AState as TdxDefaultValueBasedCommandUIState<TdxSplitTableCellsParameters>;
  if AValueState = nil then
    Exit;

  FCellsParameters := AValueState.Value;
  inherited ForceExecute(AState);
end;

procedure TdxSplitTableCellsCommand.PerformModifyModel;
var
  ASelectedCells: TdxSelectedCellsCollection;
  AFirstCell: TdxTableCell;
  AMergeCommand: TdxMergeTableCellsCommand;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ASelectedCells := TdxSelectedCellsCollection(DocumentModel.Selection.SelectedCells);
  AFirstCell := ASelectedCells.NormalizedFirst.NormalizedStartCell;

  if CellsParameters.MergeCellsBeforeSplit then
  begin
    AMergeCommand := TdxMergeTableCellsCommand.Create(RichEditControl);
    try
      AMergeCommand.PerformModifyModel;
    finally
      AMergeCommand.Free;
    end;
  end;

  SplitTableCellsHorizontally(ASelectedCells);
  SplitTableCellsVertically(ASelectedCells);

  FNewPosition := CalculateNewPosition(AFirstCell);
end;

function TdxSplitTableCellsCommand.CalculateNewPosition(AFirstCell: TdxTableCell): TdxDocumentLogPosition;
begin
  Result := ActivePieceTable.Paragraphs[AFirstCell.EndParagraphIndex].EndLogPosition;
end;

procedure TdxSplitTableCellsCommand.SplitTableCellsHorizontally(ASelectedCells: TdxSelectedCellsCollection);
var
  ADocumentServerOwner: IdxInnerRichEditDocumentServerOwner;
  AStartCell, ACurrentCell: TdxTableCell;
  ATopRowIndex, AStartCellIndex, I, J: Integer;
  AForceVisible: Boolean;
  ACurrentSelectedRow: TdxSelectedCellsIntervalInRow;
  ACells: TdxTableCellCollection;
begin
  ADocumentServerOwner := RichEditControl as IdxInnerRichEditDocumentServerOwner;
  if CellsParameters.MergeCellsBeforeSplit then
  begin
    AStartCell := ASelectedCells.NormalizedFirst.NormalizedStartCell;
    ActivePieceTable.SplitTableCellsHorizontally(AStartCell, CellsParameters.ColumnsCount, GetForceVisible, ADocumentServerOwner);
    Exit;
  end;
  ATopRowIndex := ASelectedCells.GetTopRowIndex;
  AForceVisible := GetForceVisible;
  for I := ASelectedCells.GetBottomRowIndex downto ATopRowIndex do
  begin
    ACurrentSelectedRow := ASelectedCells[I];
    ACells := ACurrentSelectedRow.Row.Cells;
    AStartCellIndex := ACurrentSelectedRow.NormalizedStartCellIndex;
    for J := ACurrentSelectedRow.NormalizedEndCellIndex downto AStartCellIndex do
    begin
      ACurrentCell := ACells[J];
      if ACurrentCell.VerticalMerging = TdxMergingState.Continue then
        Continue;
      ActivePieceTable.SplitTableCellsHorizontally(ACurrentCell, CellsParameters.ColumnsCount, AForceVisible, ADocumentServerOwner);
    end;
  end;
end;

procedure TdxSplitTableCellsCommand.SplitTableCellsVertically(ASelectedCells: TdxSelectedCellsCollection);
var
  ARowsCount, AColumnsCount: Integer;
  ASelectedFirstInterval: TdxSelectedCellsIntervalInRow;
begin
  ARowsCount := CellsParameters.RowsCount;
  if ARowsCount = 1 then
    Exit;
  ASelectedFirstInterval := ASelectedCells.NormalizedFirst;
  AColumnsCount := GetColumnsCountForSplitVertically(ASelectedFirstInterval);
  ActivePieceTable.SplitTableCellsVertically(ASelectedFirstInterval.NormalizedStartCell, ARowsCount, AColumnsCount, GetForceVisible);
end;

function TdxSplitTableCellsCommand.GetColumnsCountForSplitVertically(AInterval: TdxSelectedCellsIntervalInRow): Integer;
begin
  if CellsParameters.MergeCellsBeforeSplit then
    Result := CellsParameters.ColumnsCount
  else
    Result := (AInterval.NormalizedLength + 1) * CellsParameters.ColumnsCount;
end;

function TdxSplitTableCellsCommand.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := True;
end;

function TdxSplitTableCellsCommand.ChangePosition(const APos: TdxDocumentModelPosition): TdxDocumentLogPosition;
begin
  Result := FNewPosition;
end;

procedure TdxSplitTableCellsCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := DocumentModel.Selection.IsWholeSelectionInOneTable and
    TdxSelectionAccess(DocumentModel.Selection).IsValidSelectedCells;
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{$REGION 'TdxSelectedTableCellPositionInfo'}

{ TdxSelectedTableCellPositionInfo }

procedure TdxSelectedTableCellPositionInfo.CopyFrom(const ASource: TdxSelectedTableCellPositionInfo);
begin
  FMask := ASource.FMask;
end;

constructor TdxSelectedTableCellPositionInfo.Create(ACell: TdxTableCell);
begin
  FCell := ACell;
  FMask := [];
end;

function TdxSelectedTableCellPositionInfo.GetItem(const Index: TdxMask): Boolean;
begin
  Result := Index in FMask;
end;

procedure TdxSelectedTableCellPositionInfo.SetItem(const Index: TdxMask; const Value: Boolean);
begin
  if Value then
    Include(FMask, Index)
  else
    Exclude(FMask, Index);
end;

{$ENDREGION}

{$REGION 'TdxTableCellActualBorders'}

{ TdxTableCellActualBorders }

constructor TdxTableCellActualBorders.Create(ACell: TdxTableCell);
begin
  inherited Create;
  FCell := ACell;
end;

function TdxTableCellActualBorders.GetBottomBorder: TdxBorderBase;
begin
  if FBottom = nil then
    FBottom := FCell.GetActualBottomCellBorder;
  Result := FBottom;
end;

function TdxTableCellActualBorders.GetLeftBorder: TdxBorderBase;
begin
  if FLeft = nil then
    FLeft := FCell.GetActualLeftCellBorder;
  Result := FLeft;
end;

function TdxTableCellActualBorders.GetRightBorder: TdxBorderBase;
begin
  if FRight = nil then
    FRight := FCell.GetActualRightCellBorder;
  Result := FRight;
end;

function TdxTableCellActualBorders.GetTopBorder: TdxBorderBase;
begin
  if FTop = nil then
    FTop := FCell.GetActualTopCellBorder;
   Result := FTop;
end;

{$ENDREGION}

{$REGION 'TdxChangeTableCellsCommandBase'}

{ TdxChangeTableCellsCommandBase }

procedure TdxChangeTableCellsCommandBase.SetInsideTableBorder(AInsideBorder: TdxBorderBase; AStyle: TdxBorderLineStyle;
  AWidth: Integer; AColor: TdxAlphaColor);
begin
  AInsideBorder.BeginUpdate;
  try
    AInsideBorder.Style := AStyle;
    AInsideBorder.Width := AWidth;
    AInsideBorder.Color := AColor;
  finally
    AInsideBorder.EndUpdate;
  end;
end;

function TdxChangeTableCellsCommandBase.CanChangePosition(const APos: TdxDocumentModelPosition): Boolean;
begin
  Result := False;
end;

function TdxChangeTableCellsCommandBase.ChangePosition(const APos: TdxDocumentModelPosition): Integer;
begin
  Result := APos.LogPosition;
end;

procedure TdxChangeTableCellsCommandBase.CheckInsideTableBorder(ATable: TdxTable; AIsCheckingVerticalBorder: Boolean;
  AInsideBorder: TdxBorderBase; AStyleBorderInfo: TdxBorderInfo);
var
  I, J: Integer;
  ACurrentRow: TdxTableRow;
  ACurrentCellBorders: TdxTableCellBorders;
  ACurrentCellBorder: TdxBorderInfo;
begin
  for I := 0 to ATable.Rows.Count - 1 do
  begin
    ACurrentRow := ATable.Rows[I];
    for J := 0 to ACurrentRow.Cells.Count - 1 do
    begin
      ACurrentCellBorders := ACurrentRow.Cells[J].Properties.Borders;
      if AIsCheckingVerticalBorder then
        ACurrentCellBorder := ACurrentCellBorders.LeftBorder.Info
      else
        ACurrentCellBorder := ACurrentCellBorders.TopBorder.Info;
      if not ACurrentRow.IsFirstRowInTable and (ACurrentCellBorder.Style <> TdxBorderLineStyle.None) then
      begin
        SetInsideTableBorder(AInsideBorder, AStyleBorderInfo.Style, AStyleBorderInfo.Width, AStyleBorderInfo.Color);
        Exit;
      end;
    end;
  end;
  SetInsideTableBorder(AInsideBorder, TdxBorderLineStyle.None, 0, TdxAlphaColors.Empty);
end;

procedure TdxChangeTableCellsCommandBase.CheckInsideTableBorders;
var
  AFirstSelectedCell: TdxTableCell;
  ATable: TdxTable;
  ABorders: TdxTableBorders;
  ATableStyleProperties: TdxMergedTableProperties;
begin
  AFirstSelectedCell := DocumentModel.Selection.SelectedCells.FirstSelectedCell;
  if AFirstSelectedCell = nil then
    Exit;
  ATable := AFirstSelectedCell.Table;
  ABorders := ATable.TableProperties.Borders;
  ATableStyleProperties := ATable.TableStyle.GetMergedTableProperties;
  try
    CheckInsideTableBorder(ATable, False, ABorders.InsideHorizontalBorder, ATableStyleProperties.Info.Borders.InsideHorizontalBorder);
    CheckInsideTableBorder(ATable, True, ABorders.InsideVerticalBorder, ATableStyleProperties.Info.Borders.InsideVerticalBorder);
  finally
    ATableStyleProperties.Free;
  end;
end;

procedure TdxChangeTableCellsCommandBase.CollectCells(ATarget: TdxTableCellList);
var
  ACells: TdxSelectedCellsCollection;
begin
  if not DocumentModel.Selection.IsWholeSelectionInOneTable then
    Exit;
  ACells := GetSelectedCellsCollection;
  if ACells = nil then
    Exit;
  try
    FIsSelectedCellsSquare := ACells.SelectedOnlyOneCell or ACells.IsSquare;
    CollectCellsCore(ACells, ATarget);
  finally
    ACells.Free;
  end;
end;

procedure TdxChangeTableCellsCommandBase.CollectCell(ACell: TdxTableCell;
  var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList);
begin
  if not ATarget.Contains(ACell) then
    ATarget.Add(ACell);
end;

procedure TdxChangeTableCellsCommandBase.CollectCells(ACellsInRow: TdxSelectedCellsIntervalInRow;
  var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList);
var
  ARow: TdxTableRow;
  AStartCellIndex, AEndCellIndex: Integer;
  ACurrentCell: TdxTableCell;
  ASpanCells: TdxTableCellList;
  ASpanCellsCount: Integer;
  ACellPositionInfoForMergedCells: TdxSelectedTableCellPositionInfo;
  I, J: Integer;
begin
  ARow := ACellsInRow.Row;
  AStartCellIndex := ACellsInRow.NormalizedStartCellIndex;
  AEndCellIndex := ACellsInRow.NormalizedEndCellIndex;
  for I := AStartCellIndex to AEndCellIndex do
  begin
    ACellPositionInfo.FirstSelectedColumn := I = AStartCellIndex;
    ACellPositionInfo.LastSelectedColumn := I = AEndCellIndex;
    ACurrentCell := ARow.Cells[i];
    if IsSelectedCellsSquare then
    begin
      ASpanCells := ACurrentCell.GetVerticalSpanCells;
      try
        ASpanCellsCount := ASpanCells.Count;
        for J := 0 to ASpanCellsCount - 1 do
        begin
          ACellPositionInfoForMergedCells := TdxSelectedTableCellPositionInfo.Create(nil);
          ACellPositionInfoForMergedCells.CopyFrom(ACellPositionInfo);
          ACellPositionInfoForMergedCells.FirstSelectedRow := ACellPositionInfoForMergedCells.FirstSelectedRow and (J = 0);
          ACellPositionInfoForMergedCells.LastSelectedRow := ACellPositionInfoForMergedCells.LastSelectedRow or
            ((J = (ASpanCellsCount - 1)) and (ASpanCellsCount > 1));
          CollectCell(ASpanCells[J], ACellPositionInfoForMergedCells, ATarget);
        end;
      finally
        ASpanCells.Free;
      end;
    end
    else
      CollectCell(ACurrentCell, ACellPositionInfo, ATarget);
  end;
end;

procedure TdxChangeTableCellsCommandBase.CollectCellsCore(ACells: TdxSelectedCellsCollection;
  ATarget: TdxTableCellList);
var
  ACellPositionInfo: TdxSelectedTableCellPositionInfo;
  ACellsIsSquare, AInvert: Boolean;
  ATop, ATopRowIndex, ABottom, ABottomRowIndex, I, AIndex: Integer;
begin
  ACellPositionInfo := TdxSelectedTableCellPositionInfo.Create(nil);
  ACellsIsSquare := ACells.IsSquare;
  ATop := ACells.GetTopRowIndex;
  ATopRowIndex := ACells[ATop].Row.IndexInTable;
  ABottom := ACells.GetBottomRowIndex;
  ABottomRowIndex := ACells[ABottom].Row.IndexInTable;
  AInvert := ACells[0] <> ACells.NormalizedFirst;
  for I := ATop to ABottom do
  begin
    if AInvert then
      AIndex := ABottom - I
    else
      AIndex := I;
    ACellPositionInfo.FirstSelectedRow := not ACellsIsSquare or (ACells[AIndex].Row.IndexInTable = ATopRowIndex);
    ACellPositionInfo.LastSelectedRow := not ACellsIsSquare or (ACells[AIndex].Row.IndexInTable = ABottomRowIndex);
    CollectCells(ACells[AIndex], ACellPositionInfo, ATarget);
  end;
end;

destructor TdxChangeTableCellsCommandBase.Destroy;
begin
  FCells.Free;
  inherited Destroy;
end;

procedure TdxChangeTableCellsCommandBase.ForceExecute(const AState: IdxCommandUIState);
begin
  FExecuteState := AState;
  inherited ForceExecute(AState);
end;

function TdxChangeTableCellsCommandBase.GetExtendSelection: Boolean;
begin
  Result := False;
end;

function TdxChangeTableCellsCommandBase.GetTreatStartPositionAsCurrent: Boolean;
begin
  Result := False;
end;

function TdxChangeTableCellsCommandBase.GetTryToKeepCaretX: Boolean;
begin
  Result := False;
end;

function TdxChangeTableCellsCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.None;
end;

procedure TdxChangeTableCellsCommandBase.ModifyCells(ACells: TdxTableCellList; const AState: IdxCommandUIState);
var
  I: Integer;
begin
  for I := 0 to ACells.Count -1  do
    ModifyCell(ACells[I]);
end;

function TdxChangeTableCellsCommandBase.PerformChangeSelection: Boolean;
begin
  Result := True;
end;

procedure TdxChangeTableCellsCommandBase.PerformModifyModel;
begin
  PerformModifyModelCore(FExecuteState);
end;

procedure TdxChangeTableCellsCommandBase.PerformModifyModelCore(const AState: IdxCommandUIState);
begin
  FCells := TdxTableCellList.Create;
  CollectCells(FCells);
  ModifyCells(FCells, AState);
  CheckInsideTableBorders;
end;

procedure TdxChangeTableCellsCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  if DocumentModel.Selection.IsWholeSelectionInOneTable then
    UpdateUIStateCore(AState, DocumentModel.Selection.SelectedCells)
  else
    AState.Enabled := False;
end;

procedure TdxChangeTableCellsCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState;
  const ASelectedCellsCollection: TdxSelectedTableStructureBase);
begin
  ApplyCommandRestrictionOnEditableControl(AState, Options.DocumentCapabilities.Tables, DocumentModel.Selection.IsWholeSelectionInOneTable);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsBordersCommandBase'}

{ TdxToggleTableCellsBordersCommandBase }

destructor TdxToggleTableCellsBordersCommandBase.Destroy;
begin
  FBorders.Free;
  FActualBorders.Free;
  inherited Destroy;
end;

function TdxToggleTableCellsBordersCommandBase.GetActualNewBorder: TdxBorderInfo;
begin
  if Assigned(FNewBorder) then
    Result := FNewBorder
  else
    Result := DocumentModel.TableBorderInfoRepository.CurrentItem;
end;

procedure TdxToggleTableCellsBordersCommandBase.ModifyBorder(ABorder: TdxBorderBase);
var
  ABorderInfo: TdxBorderInfo;
begin
  ABorderInfo := GetActualNewBorder;
  ABorder.BeginUpdate;
  try
    ABorder.Style := ABorderInfo.Style;
    ABorder.Width := ABorderInfo.Width;
    ABorder.Color := ABorderInfo.Color;
  finally
    ABorder.EndUpdate;
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.ModifyCell(ACell: TdxTableCell);
begin
end;

procedure TdxToggleTableCellsBordersCommandBase.ModifyCells(ACells: TdxTableCellList; const AState: IdxCommandUIState);
var
  I: Integer;
begin
  if AState.Checked then
    for I := 0 to FBorders.Count - 1 do
      ResetBorder(FBorders[I])
  else
    for I := 0 to FBorders.Count - 1 do
      ModifyBorder(FBorders[I]);
end;

procedure TdxToggleTableCellsBordersCommandBase.ResetBorder(ABorder: TdxBorderBase);
begin
  ABorder.ReplaceInfo(TdxBorderInfo.Empty, ABorder.BatchUpdateChangeActions);
end;

procedure TdxToggleTableCellsBordersCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
var
  ACells: TdxTableCellList;
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
  begin
    ACells := TdxTableCellList.Create;
    try
      CollectCells(ACells);
      AState.Checked := CalculateIsChecked(FActualBorders);
    finally
      ACells.Free;
    end;
  end;
end;

function TdxToggleTableCellsBordersCommandBase.AreTableBordersAffected: Boolean;
var
  ASelection: TdxSelection;
  ACells: TdxSelectedCellsCollection;
begin
  ASelection := DocumentModel.Selection;
  ACells := TdxSelectedCellsCollection(ASelection.SelectedCells);
  Result := ACells.IsSelectedEntireTable and ASelection.IsWholeSelectionInOneTable;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectCells(ATarget: TdxTableCellList);
var
  ATalbeBorders: TdxTableBorders;
begin
  FreeAndNil(FBorders);
  FBorders := TdxBorderBaseList.Create;
  FreeAndNil(FActualBorders);
  FActualBorders := TdxBorderBaseList.Create;
  if AreTableBordersAffected then
  begin
    ATalbeBorders := DocumentModel.Selection.SelectedCells.FirstSelectedCell.Table.TableProperties.Borders;
    CollectTableBorders(ATalbeBorders, FBorders);
  end;
  inherited CollectCells(ATarget);
end;

function TdxToggleTableCellsBordersCommandBase.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
var
  ACount: Integer;
  AInfo, AInitialInfo: TdxBorderInfo;
  I: Integer;
begin
  ACount := ABorders.Count;
  if ACount <= 0 then
    Exit(False);

  AInitialInfo := GetActualNewBorder;
  for I := 0 to ACount - 1 do
  begin
    AInfo := ABorders[I].Info;
    if not AInfo.Equals(AInitialInfo) then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectBottomNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
var
  ARow: TdxTableRow;
  ACells, ASpanCells: TdxTableCellList;
  I: Integer;
begin
  if IsSelectedCellsSquare then
  begin
    ASpanCells := ACellsInRow.StartCell.GetVerticalSpanCells;
    try
      ARow := ASpanCells[ASpanCells.Count - 1].Row;
    finally
      ASpanCells.Free;
    end;
  end
  else
    ARow := ACellsInRow.Row;

  if ARow.IsLastRowInTable then
    Exit;

  ACells := TdxTableCellList.Create;
  try
    CollectNeighbourRowCells(ARow.Next, ACellsInRow, ACells);
    for I := 0 to ACells.Count - 1 do
      ABorders.Add(ACells[I].Properties.Borders.TopBorder);
  finally
    ACells.Free;
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectCell(ATableCell: TdxTableCell;
  var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList);
var
  ACellActualBorders: TdxTableCellActualBorders;
begin
  CollectBorders(ATableCell.Properties.Borders, ACellPositionInfo, FBorders);
  ACellActualBorders := TdxTableCellActualBorders.Create(ATableCell);
  try
    CollectBorders(ACellActualBorders, ACellPositionInfo, FActualBorders);
  finally
    ACellActualBorders.Free;
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectCells(ACellsInRow: TdxSelectedCellsIntervalInRow;
  var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList);
var
  AHasCellSpacing: Boolean;
begin
  AHasCellSpacing := ACellsInRow.Table.CellSpacing.Value > 0;
  if not AHasCellSpacing then
    CollectLeftNeighbourBorders(ACellsInRow, FBorders);
  inherited CollectCells(ACellsInRow, ACellPositionInfo, ATarget);
  if not AHasCellSpacing then
    CollectRightNeighbourBorders(ACellsInRow, FBorders);
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectCellsCore(ACells: TdxSelectedCellsCollection;
  ATarget: TdxTableCellList);
var
  AItems: TdxSelectionItemList;
begin
  if not ACells.IsNotEmpty then
    Exit;
  AItems := TdxDocumentModel(ACells.FirstSelectedCell.DocumentModel).Selection.Items;
  if AItems[0].Generation = AItems[AItems.Count - 1].Generation then
  begin
    CollectTopAndBottomNeighbourBordersCore(ACells, ATarget, ACells.NormalizedFirst, ACells.NormalizedLast);
    inherited CollectCellsCore(ACells, ATarget);
  end
  else
    CollectCellsCoreWithMultipleItems(ACells, ATarget);
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectCellsCoreWithMultipleItems(
  ASelectedCells: TdxSelectedCellsCollection; ATarget: TdxTableCellList);
begin
  CollectTopAndBottomNeighbourBorders(ASelectedCells, ATarget);
  inherited CollectCellsCore(ASelectedCells, ATarget);
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
var
  ACell, APreviousCell: TdxTableCell;
begin
  ACell := ACellsInRow.NormalizedStartCell;
  if IsSelectedCellsSquare then
  begin
    CollectLeftNeighbourBordersInMergedCells(ACell, ABorders);
    Exit;
  end;

  if ACell.IsFirstCellInRow then
    Exit;

  APreviousCell := ACell.Previous;
  FBorders.Add(APreviousCell.Properties.Borders.RightBorder);
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectLeftNeighbourBordersInMergedCells(ACell: TdxTableCell;
  ABorders: TdxBorderBaseList);
var
  ASpanCells: TdxTableCellList;
  ASpanCellsCount: Integer;
  ACurrentCell, APreviousCell: TdxTableCell;
  ARightBorder: TdxRightBorder;
  I: Integer;
begin
  ASpanCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACell, ACell.GetStartColumnIndexConsiderRowGrid, False);
  try
    ASpanCellsCount := ASpanCells.Count;
    for I := 0 to ASpanCellsCount - 1 do
    begin
      ACurrentCell := ASpanCells[I];
      if ACell.IsFirstCellInRow then
        Continue;

      APreviousCell := ACurrentCell.Previous;
      ARightBorder := APreviousCell.Properties.Borders.RightBorder;
      if not FBorders.Contains(ARightBorder) then
        FBorders.Add(ARightBorder);
    end;
  finally
    ASpanCells.Free;
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectNeighbourRowCells(ARow: TdxTableRow;
  ACellsInRow: TdxSelectedCellsIntervalInRow; ATarget: TdxTableCellList);
var
  AStartColumnIndex, AEndColumnIndex: Integer;
  ACells: TdxTableCellList;
  AFirstCell, ALastCell: TdxTableCell;
  ACount: Integer;
begin
  AStartColumnIndex := ACellsInRow.NormalizedStartCell.GetStartColumnIndexConsiderRowGrid;
  AEndColumnIndex := ACellsInRow.NormalizedEndCell.GetEndColumnIndexConsiderRowGrid;
  ACells := TdxTableCellVerticalBorderCalculator.GetCellsByIntervalColumnIndex(ARow, AStartColumnIndex, AEndColumnIndex);
  try
    ACount := ACells.Count;
    if ACount <= 0 then
      Exit;

    ALastCell := ACells[ACount - 1];
    if ALastCell.GetEndColumnIndexConsiderRowGrid > AEndColumnIndex then
      ACells.Delete(ACount - 1);
    if ACells.Count = 0 then
      Exit;
    AFirstCell := ACells[0];
    if AFirstCell.GetStartColumnIndexConsiderRowGrid < AStartColumnIndex then
      ACells.Delete(0);
    ATarget.AddRange(ACells);
  finally
    ACells.Free;
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
var
  ACell, ANextCell: TdxTableCell;
begin
  ACell := ACellsInRow.NormalizedEndCell;
  if IsSelectedCellsSquare then
  begin
    CollectRightNeighbourBordersInMergedCells(ACell, ABorders);
    Exit;
  end;

  if ACell.IsLastCellInRow then
    Exit;

  ANextCell := ACell.Next;
  ABorders.Add(ANextCell.Properties.Borders.LeftBorder);
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectRightNeighbourBordersInMergedCells(aCell: TdxTableCell;
  ABorders: TdxBorderBaseList);
var
  ASpanCells: TdxTableCellList;
  ACurrentCell, ANextCell: TdxTableCell;
  ALeftBorder: TdxLeftBorder;
  I: Integer;
begin
  ASpanCells := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(ACell, ACell.GetStartColumnIndexConsiderRowGrid, False);
  try
    for I := 0 to ASpanCells.Count - 1 do
    begin
      ACurrentCell := ASpanCells[I];
      if ACurrentCell.IsLastCellInRow then
        Continue;

      ANextCell := ACurrentCell.Next;
      ALeftBorder := ANextCell.Properties.Borders.LeftBorder;
      if not FBorders.Contains(ALeftBorder) then
        FBorders.Add(ALeftBorder);
    end;
  finally
    ASpanCells.Free;
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectTopAndBottomNeighbourBorders(
  ASelectedCells: TdxSelectedCellsCollection; ATarget: TdxTableCellList);
var
  AItems: TdxSelectionItemList;
  AGenerationStartCellIndex, ACount: Integer;
  I: Integer;
begin
  AItems := DocumentModel.Selection.Items;
  AGenerationStartCellIndex := 0;
  ACount := AItems.Count;
  Assert(ACount = ASelectedCells.RowsCount);
  for I := 0 to ACount - 1 do
    if (I = ACount - 1) or (AItems[I].Generation <> AItems[I + 1].Generation) then
    begin
      CollectTopAndBottomNeighbourBordersCore(ASelectedCells, ATarget, ASelectedCells[AGenerationStartCellIndex], ASelectedCells[I]);
      AGenerationStartCellIndex := I + 1;
    end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectTopAndBottomNeighbourBordersCore(
  ACells: TdxSelectedCellsCollection; ATarget: TdxTableCellList; AFirst, ALast: TdxSelectedCellsIntervalInRow);
begin
  if not (ACells.First.Table.CellSpacing.Value > 0) then
  begin
    CollectTopNeighbourBorders(AFirst, FBorders);
    CollectBottomNeighbourBorders(ALast, FBorders);
  end;
end;

procedure TdxToggleTableCellsBordersCommandBase.CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
var
  ACells: TdxTableCellList;
  I: Integer;
begin
  if ACellsInRow.Row.IsFirstRowInTable then
    Exit;

  ACells := TdxTableCellList.Create;
  try
    CollectNeighbourRowCells(ACellsInRow.Row.Previous, ACellsInRow, ACells);
    for I := 0 to ACells.Count - 1 do
      FBorders.Add(ACells[I].Properties.Borders.BottomBorder);
  finally
    ACells.Free;
  end;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsBottomBorderCommand'}

{ TdxToggleTableCellsBottomBorderCommand }

procedure TdxToggleTableCellsBottomBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if ACellPositionInfo.LastSelectedRow then
    ABorders.Add(ACellBorders.BottomBorder);
end;

procedure TdxToggleTableCellsBottomBorderCommand.CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsBottomBorderCommand.CollectRightNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsBottomBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ATableBorders.BottomBorder);
end;

procedure TdxToggleTableCellsBottomBorderCommand.CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

class function TdxToggleTableCellsBottomBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomBorderDescription);
end;

class function TdxToggleTableCellsBottomBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomBorderMenuCaption);
end;

class function TdxToggleTableCellsBottomBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsBottomBorder;
end;

class function TdxToggleTableCellsBottomBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderBottom;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsTopBorderCommand'}

{ TdxToggleTableCellsTopBorderCommand }

procedure TdxToggleTableCellsTopBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if ACellPositionInfo.FirstSelectedRow then
      ABorders.Add(ACellBorders.TopBorder);
end;

procedure TdxToggleTableCellsTopBorderCommand.CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsTopBorderCommand.CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsTopBorderCommand.CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsTopBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ATableBorders.TopBorder);
end;

class function TdxToggleTableCellsTopBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopBorderDescription);
end;

class function TdxToggleTableCellsTopBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopBorderMenuCaption);
end;

class function TdxToggleTableCellsTopBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsTopBorder;
end;

class function TdxToggleTableCellsTopBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderTop;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsInsideHorizontalBorderCommand'}

{ TdxToggleTableCellsInsideHorizontalBorderCommand }

procedure TdxToggleTableCellsInsideHorizontalBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if not ACellPositionInfo.FirstSelectedRow then
      ABorders.Add(ACellBorders.TopBorder);
  if not ACellPositionInfo.LastSelectedRow then
      ABorders.Add(ACellBorders.BottomBorder);
end;

procedure TdxToggleTableCellsInsideHorizontalBorderCommand.CollectBottomNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideHorizontalBorderCommand.CollectLeftNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideHorizontalBorderCommand.CollectRightNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideHorizontalBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideHorizontalBorderCommand.CollectTopNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

class function TdxToggleTableCellsInsideHorizontalBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsInsideHorizontalBorderDescription);
end;

class function TdxToggleTableCellsInsideHorizontalBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsInsideHorizontalBorderMenuCaption);
end;

class function TdxToggleTableCellsInsideHorizontalBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsInsideHorizontalBorder;
end;

class function TdxToggleTableCellsInsideHorizontalBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderInsideHorizontal;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsLeftBorderCommand'}

{ TdxToggleTableCellsLeftBorderCommand }

procedure TdxToggleTableCellsLeftBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if ACellPositionInfo.FirstSelectedColumn then
    ABorders.Add(ACellBorders.LeftBorder);
end;

procedure TdxToggleTableCellsLeftBorderCommand.CollectBottomNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsLeftBorderCommand.CollectRightNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsLeftBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ATableBorders.LeftBorder);
end;

procedure TdxToggleTableCellsLeftBorderCommand.CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

class function TdxToggleTableCellsLeftBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsLeftBorderDescription);
end;

class function TdxToggleTableCellsLeftBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsLeftBorderMenuCaption);
end;

class function TdxToggleTableCellsLeftBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsLeftBorder;
end;

class function TdxToggleTableCellsLeftBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderLeft;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsRightBorderCommand'}

{ TdxToggleTableCellsRightBorderCommand }

procedure TdxToggleTableCellsRightBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if ACellPositionInfo.LastSelectedColumn then
    ABorders.Add(ACellBorders.RightBorder);
end;

procedure TdxToggleTableCellsRightBorderCommand.CollectBottomNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsRightBorderCommand.CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsRightBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ATableBorders.RightBorder);
end;

procedure TdxToggleTableCellsRightBorderCommand.CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

class function TdxToggleTableCellsRightBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsRightBorderDescription);
end;

class function TdxToggleTableCellsRightBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsRightBorderMenuCaption);
end;

class function TdxToggleTableCellsRightBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsRightBorder;
end;

class function TdxToggleTableCellsRightBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderRight;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsInsideVerticalBorderCommand'}

{ TdxToggleTableCellsInsideVerticalBorderCommand }

class function TdxToggleTableCellsInsideVerticalBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsInsideVerticalBorderDescription);
end;

class function TdxToggleTableCellsInsideVerticalBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsInsideVerticalBorderMenuCaption);
end;

class function TdxToggleTableCellsInsideVerticalBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsInsideVerticalBorder;
end;

class function TdxToggleTableCellsInsideVerticalBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderInsideVertical;
end;

procedure TdxToggleTableCellsInsideVerticalBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if not ACellPositionInfo.FirstSelectedColumn then
    ABorders.Add(ACellBorders.LeftBorder);
  if not ACellPositionInfo.LastSelectedColumn then
    ABorders.Add(ACellBorders.RightBorder);
end;

procedure TdxToggleTableCellsInsideVerticalBorderCommand.CollectBottomNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideVerticalBorderCommand.CollectLeftNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideVerticalBorderCommand.CollectRightNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideVerticalBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideVerticalBorderCommand.CollectTopNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsAllBordersCommand'}

{ TdxToggleTableCellsAllBordersCommand }

class function TdxToggleTableCellsAllBordersCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsAllBordersDescription);
end;

class function TdxToggleTableCellsAllBordersCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsAllBordersMenuCaption);
end;

class function TdxToggleTableCellsAllBordersCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsAllBorders;
end;

class function TdxToggleTableCellsAllBordersCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BordersAll;
end;

procedure TdxToggleTableCellsAllBordersCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ACellBorders.LeftBorder);
  ABorders.Add(ACellBorders.RightBorder);
  ABorders.Add(ACellBorders.TopBorder);
  ABorders.Add(ACellBorders.BottomBorder);
end;

procedure TdxToggleTableCellsAllBordersCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ATableBorders.LeftBorder);
  ABorders.Add(ATableBorders.TopBorder);
  ABorders.Add(ATableBorders.RightBorder);
  ABorders.Add(ATableBorders.BottomBorder);
end;

{$ENDREGION}

{$REGION 'TdxResetTableCellsBordersCommand'}

{ TdxResetTableCellsBordersCommand }

class function TdxResetTableCellsBordersCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandResetTableCellsBordersDescription);
end;

class function TdxResetTableCellsBordersCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandResetTableCellsBordersMenuCaption);
end;

class function TdxResetTableCellsBordersCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ResetTableCellsAllBorders;
end;

class function TdxResetTableCellsBordersCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BorderNone;
end;

function TdxResetTableCellsBordersCommand.CalculateIsChecked(ABorders: TdxBorderBaseList): Boolean;
begin
  Result := True;
end;
{$ENDREGION}

{$REGION 'TdxToggleTableCellsOutsideBorderCommand'}

{ TdxToggleTableCellsOutsideBorderCommand }

class function TdxToggleTableCellsOutsideBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsOutsideBorderDescription);
end;

class function TdxToggleTableCellsOutsideBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsOutsideBorderMenuCaption);
end;

class function TdxToggleTableCellsOutsideBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsOutsideBorder;
end;

class function TdxToggleTableCellsOutsideBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BordersOutside;
end;

procedure TdxToggleTableCellsOutsideBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if ACellPositionInfo.FirstSelectedColumn then
    ABorders.Add(ACellBorders.LeftBorder);
  if ACellPositionInfo.LastSelectedColumn then
    ABorders.Add(ACellBorders.RightBorder);
  if ACellPositionInfo.FirstSelectedRow then
    ABorders.Add(ACellBorders.TopBorder);
  if ACellPositionInfo.LastSelectedRow then
    ABorders.Add(ACellBorders.BottomBorder);
end;

procedure TdxToggleTableCellsOutsideBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
  ABorders.Add(ATableBorders.LeftBorder);
  ABorders.Add(ATableBorders.TopBorder);
  ABorders.Add(ATableBorders.RightBorder);
  ABorders.Add(ATableBorders.BottomBorder);
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsInsideBorderCommand'}

{ TdxToggleTableCellsInsideBorderCommand }

class function TdxToggleTableCellsInsideBorderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsInsideBorderDescription);
end;

class function TdxToggleTableCellsInsideBorderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsInsideBorderMenuCaption);
end;

class function TdxToggleTableCellsInsideBorderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsInsideBorder;
end;

class function TdxToggleTableCellsInsideBorderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.BordersInside;
end;

procedure TdxToggleTableCellsInsideBorderCommand.CollectBorders(const ACellBorders: IdxTableCellBorders;
  ACellPositionInfo: TdxSelectedTableCellPositionInfo; ABorders: TdxBorderBaseList);
begin
  if not ACellPositionInfo.FirstSelectedColumn then
    ABorders.Add(ACellBorders.LeftBorder);
  if not ACellPositionInfo.LastSelectedColumn then
    ABorders.Add(ACellBorders.RightBorder);
  if not ACellPositionInfo.FirstSelectedRow then
    ABorders.Add(ACellBorders.TopBorder);
  if not ACellPositionInfo.LastSelectedRow then
    ABorders.Add(ACellBorders.BottomBorder);
end;

procedure TdxToggleTableCellsInsideBorderCommand.CollectBottomNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideBorderCommand.CollectLeftNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideBorderCommand.CollectRightNeighbourBorders(
  ACellsInRow: TdxSelectedCellsIntervalInRow; ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideBorderCommand.CollectTableBorders(ATableBorders: TdxTableBorders;
  ABorders: TdxBorderBaseList);
begin
end;

procedure TdxToggleTableCellsInsideBorderCommand.CollectTopNeighbourBorders(ACellsInRow: TdxSelectedCellsIntervalInRow;
  ABorders: TdxBorderBaseList);
begin
end;

{$ENDREGION}

{ TdxToggleTableCellsContentAlignmentCommandBase }

procedure TdxToggleTableCellsContentAlignmentCommandBase.CollectCellsCore(ACells: TdxSelectedCellsCollection; ATarget: TdxTableCellList);
begin
  FCellsContentIntervals.Free;
  FCellsContentIntervals := TdxSelectionItems.Create;
  inherited CollectCellsCore(ACells, ATarget);
end;

destructor TdxToggleTableCellsContentAlignmentCommandBase.Destroy;
begin
  FCellsContentIntervals.Free;
  inherited Destroy;
end;

procedure TdxToggleTableCellsContentAlignmentCommandBase.CollectCell(ACell: TdxTableCell; var ACellPositionInfo: TdxSelectedTableCellPositionInfo; ATarget: TdxTableCellList);
var
  AItem: TdxSelectionItem;
  AEndParagraph: TdxParagraph;
begin
  inherited CollectCell(ACell, ACellPositionInfo, ATarget);

  AItem := TdxSelectionItem.Create(ActivePieceTable);
  AItem.BeginUpdate;
  try
    AEndParagraph := ActivePieceTable.Paragraphs[ACell.EndParagraphIndex];
    AItem.Start := ActivePieceTable.Paragraphs[ACell.StartParagraphIndex].LogPosition;
    AItem.&End := AEndParagraph.LogPosition + AEndParagraph.Length;
  finally
    AItem.EndUpdate;
  end;

  FCellsContentIntervals.Add(AItem);
end;

procedure TdxToggleTableCellsContentAlignmentCommandBase.PerformModifyModelCore(const AState: IdxCommandUIState);
var
  ACommand: TdxToggleTableCellsParagraphAlignmentCommand;
  AParagraphAlignmentState: IdxCommandUIState;
begin
  inherited PerformModifyModelCore(AState);

  ACommand := TdxToggleTableCellsParagraphAlignmentCommand.Create(RichEditControl, Self);
  try
    AParagraphAlignmentState := ACommand.CreateDefaultCommandUIState;
    ACommand.ModifyDocumentModelCore(AParagraphAlignmentState);
  finally
    ACommand.Free;
  end;
end;

procedure TdxToggleTableCellsContentAlignmentCommandBase.ModifyCell(ATableCell: TdxTableCell);
begin
  ATableCell.VerticalAlignment := CellVerticalAlignment;
end;

procedure TdxToggleTableCellsContentAlignmentCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState; const ASelectedCellsCollection: TdxSelectedTableStructureBase);
var
  ACells: TdxTableCellList;
  ASelectedCells: TdxSelectedCellsCollection;
  ACount, I: Integer;
  AVerticalAlignment: TdxVerticalAlignment;
  ACommand: TdxToggleTableCellsParagraphAlignmentCommand;
  AHorizontalAlignmentState: IdxCommandUIState;
begin
  inherited UpdateUIStateCore(AState, ASelectedCellsCollection);

  if not AState.Enabled then
    Exit;

  ACells := TdxTableCellList.Create;
  try
    if ASelectedCellsCollection is TdxSelectedCellsCollection then
    begin
      ASelectedCells := TdxSelectedCellsCollection(ASelectedCellsCollection);
      CollectCellsCore(ASelectedCells, ACells);
    end;

    ACount := ACells.Count;
    if ACount <= 0 then
    begin
      AState.Enabled := False;
      AState.Checked := False;
      Exit;
    end;

    AVerticalAlignment := ACells[0].VerticalAlignment;
    for I := 1 to ACount - 1 do
    begin
      if ACells[I].VerticalAlignment <> AVerticalAlignment then
      begin
        AState.Checked := False;
        Exit;
      end;
    end;
  finally
    ACells.Free;
  end;

  ACommand := TdxToggleTableCellsParagraphAlignmentCommand.Create(RichEditControl, Self);
  try
    AHorizontalAlignmentState := ACommand.CreateDefaultCommandUIState;
    ACommand.UpdateUIState(AHorizontalAlignmentState);
    if not AHorizontalAlignmentState.Checked then
    begin
      AState.Checked := False;
      Exit;
    end;
  finally
    ACommand.Free;
  end;

  AState.Checked := AVerticalAlignment = CellVerticalAlignment;
end;

{ TdxToggleTableCellsParagraphAlignmentCommand }

constructor TdxToggleTableCellsParagraphAlignmentCommand.Create(const AControl: IdxRichEditControl; AOwnerCommand: TdxToggleTableCellsContentAlignmentCommandBase);
begin
  inherited Create(AControl);
  FOwnerCommand := AOwnerCommand;
end;

class function TdxToggleTableCellsParagraphAlignmentCommand.GetMenuCaption: string;
begin
  Result := 'InternalError';
end;

class function TdxToggleTableCellsParagraphAlignmentCommand.GetDescription: string;
begin
  Result := 'InternalError';
end;

function TdxToggleTableCellsParagraphAlignmentCommand.GetSelectionItems: TdxSelectionItems;
begin
  Result := FOwnerCommand.CellsContentIntervals;
end;

function TdxToggleTableCellsParagraphAlignmentCommand.CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<TdxParagraphAlignment>;
var
  AAlignment: TdxParagraphAlignment;
begin
  if AState.Checked then
    AAlignment := TdxParagraphAlignment.Left
  else
    AAlignment := FOwnerCommand.ParagraphAlignment;

  Result := TdxParagraphAlignmentModifier.Create(AAlignment);
end;

function TdxToggleTableCellsParagraphAlignmentCommand.IsCheckedValue(AValue: TdxParagraphAlignment): Boolean;
begin
  Result := AValue = FOwnerCommand.ParagraphAlignment;
end;

{ TdxChangeTableCellsContentAlignmentPlaceholderCommand }

class function TdxChangeTableCellsContentAlignmentPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeTableCellsContentAlignmentDescription);
end;

class function TdxChangeTableCellsContentAlignmentPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeTableCellsContentAlignmentMenuCaption);
end;

class function TdxChangeTableCellsContentAlignmentPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeTableCellsContentAlignmentPlaceholder;
end;

procedure TdxChangeTableCellsContentAlignmentPlaceholderCommand.ForceExecute(const AState: IdxCommandUIState);
begin
end;

function TdxChangeTableCellsContentAlignmentPlaceholderCommand.CreateDefaultCommandUIState: IdxCommandUIState;
begin
  Result := TdxDefaultCommandUIState.Create;
end;

procedure TdxChangeTableCellsContentAlignmentPlaceholderCommand.ModifyCell(ATableCell: TdxTableCell);
begin
end;

procedure TdxChangeTableCellsContentAlignmentPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  AState.Visible := AState.Enabled;
end;

{$REGION 'TdxToggleTableCellsTopLeftAlignmentCommand'}

{ TdxToggleTableCellsTopLeftAlignmentCommand }

class function TdxToggleTableCellsTopLeftAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopLeftAlignmentDescription);
end;

class function TdxToggleTableCellsTopLeftAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopLeftAlignmentMenuCaption);
end;

class function TdxToggleTableCellsTopLeftAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsTopLeftAlignment;
end;

class function TdxToggleTableCellsTopLeftAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentTopLeft;
end;

function TdxToggleTableCellsTopLeftAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Top;
end;

function TdxToggleTableCellsTopLeftAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Left;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsTopCenterAlignmentCommand'}

{ TdxToggleTableCellsTopCenterAlignmentCommand }

class function TdxToggleTableCellsTopCenterAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopCenterAlignmentDescription);
end;

class function TdxToggleTableCellsTopCenterAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopCenterAlignmentMenuCaption);
end;

class function TdxToggleTableCellsTopCenterAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsTopCenterAlignment;
end;

class function TdxToggleTableCellsTopCenterAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentTopCenter;
end;

function TdxToggleTableCellsTopCenterAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Center;
end;

function TdxToggleTableCellsTopCenterAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Top;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsTopRightAlignmentCommand'}

{ TdxToggleTableCellsTopRightAlignmentCommand }

class function TdxToggleTableCellsTopRightAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopRightAlignmentDescription);
end;

class function TdxToggleTableCellsTopRightAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsTopRightAlignmentMenuCaption);
end;

class function TdxToggleTableCellsTopRightAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsTopRightAlignment;
end;

class function TdxToggleTableCellsTopRightAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentTopRight;
end;

function TdxToggleTableCellsTopRightAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Right;
end;

function TdxToggleTableCellsTopRightAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Top;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsMiddleLeftAlignmentCommand'}

{ TdxToggleTableCellsMiddleLeftAlignmentCommand }

class function TdxToggleTableCellsMiddleLeftAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentDescription);
end;

class function TdxToggleTableCellsMiddleLeftAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsMiddleLeftAlignmentMenuCaption);
end;

class function TdxToggleTableCellsMiddleLeftAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsMiddleLeftAlignment;
end;

class function TdxToggleTableCellsMiddleLeftAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentMiddleLeft;
end;

function TdxToggleTableCellsMiddleLeftAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Left;
end;

function TdxToggleTableCellsMiddleLeftAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Center;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsMiddleCenterAlignmentCommand'}

{ TdxToggleTableCellsMiddleCenterAlignmentCommand }

class function TdxToggleTableCellsMiddleCenterAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentDescription);
end;

class function TdxToggleTableCellsMiddleCenterAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsMiddleCenterAlignmentMenuCaption);
end;

class function TdxToggleTableCellsMiddleCenterAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsMiddleCenterAlignment;
end;

class function TdxToggleTableCellsMiddleCenterAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentMiddleCenter;
end;

function TdxToggleTableCellsMiddleCenterAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Center;
end;

function TdxToggleTableCellsMiddleCenterAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Center;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsMiddleRightAlignmentCommand'}

{ TdxToggleTableCellsMiddleRightAlignmentCommand }

class function TdxToggleTableCellsMiddleRightAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsMiddleRightAlignmentDescription);
end;

class function TdxToggleTableCellsMiddleRightAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsMiddleRightAlignmentMenuCaption);
end;

class function TdxToggleTableCellsMiddleRightAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsMiddleRightAlignment;
end;

class function TdxToggleTableCellsMiddleRightAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentMiddleRight;
end;

function TdxToggleTableCellsMiddleRightAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Center;
end;

function TdxToggleTableCellsMiddleRightAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Right;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsBottomLeftAlignmentCommand'}

{ TdxToggleTableCellsBottomLeftAlignmentCommand }

class function TdxToggleTableCellsBottomLeftAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomLeftAlignmentDescription);
end;

class function TdxToggleTableCellsBottomLeftAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomLeftAlignmentMenuCaption);
end;

class function TdxToggleTableCellsBottomLeftAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsBottomLeftAlignment;
end;

class function TdxToggleTableCellsBottomLeftAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentBottomLeft;
end;

function TdxToggleTableCellsBottomLeftAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Bottom;
end;

function TdxToggleTableCellsBottomLeftAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Left;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsBottomCenterAlignmentCommand'}

{ TdxToggleTableCellsBottomCenterAlignmentCommand }

class function TdxToggleTableCellsBottomCenterAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomCenterAlignmentDescription);
end;

class function TdxToggleTableCellsBottomCenterAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomCenterAlignmentMenuCaption);
end;

class function TdxToggleTableCellsBottomCenterAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsBottomCenterAlignment;
end;

class function TdxToggleTableCellsBottomCenterAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentBottomCenter;
end;

function TdxToggleTableCellsBottomCenterAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Bottom;
end;

function TdxToggleTableCellsBottomCenterAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Center;
end;

{$ENDREGION}

{$REGION 'TdxToggleTableCellsBottomRightAlignmentCommand'}

{ TdxToggleTableCellsBottomRightAlignmentCommand }

class function TdxToggleTableCellsBottomRightAlignmentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomRightAlignmentDescription);
end;

class function TdxToggleTableCellsBottomRightAlignmentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleTableCellsBottomRightAlignmentMenuCaption);
end;

class function TdxToggleTableCellsBottomRightAlignmentCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleTableCellsBottomRightAlignment;
end;

class function TdxToggleTableCellsBottomRightAlignmentCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.CellsAlignmentBottomRight;
end;

function TdxToggleTableCellsBottomRightAlignmentCommand.GetCellVerticalAlignment: TdxVerticalAlignment;
begin
  Result := TdxVerticalAlignment.Bottom;
end;

function TdxToggleTableCellsBottomRightAlignmentCommand.GetParagraphAlignment: TdxParagraphAlignment;
begin
  Result := TdxParagraphAlignment.Right;
end;

{$ENDREGION}

end.
