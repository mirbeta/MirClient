{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridInplaceEditForm;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Graphics, Controls, Contnrs, ImgList, StdCtrls,
  dxCore, cxClasses, cxGraphics, cxControls, cxStyles, cxLookAndFeelPainters,
  cxGridCommon, cxGrid, dxCoreClasses, dxGDIPlusClasses,
  cxGridCustomView, cxGridCustomTableView, dxLayoutLookAndFeels, cxDataStorage,
  cxCustomData, cxEdit, dxLayoutContainer, dxLayoutSelection,
  dxLayoutCommon, Forms, cxNavigator, cxLookAndFeels, SysUtils,
  cxGridViewLayoutContainer, cxMaskEdit;

type
  TcxGridEditMode = (emInplace, emInplaceEditForm, emInplaceEditFormHideCurrentRow);
  TcxGridInplaceEditFormStretch = (fsNone, fsHorizontal, fsVertical, fsClient);
  TcxGridMasterRowDblClickAction = (dcaSwitchExpandedState, dcaShowEditForm);

const
  cxGridInplaceEditFormLayoutItemEditDefaultMinValueWidth = 80;
  cxGridInplaceEditFormEditingRecordIndexNone = cxNullEditingRecordIndex;
  cxGridInplaceEditFormDefaultColumnCount = 2;
  cxGridInplaceEditFormDefaultMasterRowDblClickAction = dcaSwitchExpandedState;
  cxGridInplaceEditFormDefaultEditMode = emInplace;

type
  TcxGridInplaceEditFormLayoutItem = class;
  TcxGridInplaceEditFormBaseLayoutItem = class;
  TcxGridInplaceEditFormLayoutItemEditViewInfo = class;
  TcxGridInplaceEditFormLayoutItemViewInfo = class;
  TcxGridInplaceEditForm = class;
  TcxGridInplaceEditFormContainerViewInfo = class;
  TcxGridInplaceEditFormController = class;
  TcxGridInplaceEditFormContainer = class;
  TcxGridInplaceEditFormDataCellViewInfo = class;

  TcxGridInplaceEditFormLayoutItemClass = class of TcxGridInplaceEditFormLayoutItem;
  TcxGridInplaceEditFormControllerClass = class of TcxGridInplaceEditFormController;
  TcxGridInplaceEditFormContainerPainterClass = class of TcxGridInplaceEditFormContainerPainter;
  TcxGridInplaceEditFormContainerViewInfoClass = class of TcxGridInplaceEditFormContainerViewInfo;
  TcxGridInplaceEditFormContainerClass = class of TcxGridInplaceEditFormContainer;
  TcxGridInplaceEditFormClass = class of TcxGridInplaceEditForm;

  { TcxGridInplaceEditFormLayoutLookAndFeel }

  TcxGridInplaceEditFormLayoutLookAndFeel = class(TcxGridCustomLayoutLookAndFeel)
  public
    constructor Create(AGridView: TcxCustomGridTableView); override;
  end;

  { TcxGridInplaceEditFormLayoutItemEditPainter }

  TcxGridInplaceEditFormLayoutItemEditPainter = class(TcxGridCustomLayoutItemEditPainter)
  private
    function GetViewInfo: TcxGridInplaceEditFormLayoutItemEditViewInfo;
  protected
    procedure DrawEdit(ACanvas: TcxCanvas); override;
  public
    property ViewInfo: TcxGridInplaceEditFormLayoutItemEditViewInfo read GetViewInfo;
  end;

  { TcxGridInplaceEditFormLayoutItemPainter }

  TcxGridInplaceEditFormLayoutItemPainter = class(TcxGridCustomLayoutItemPainter)
  protected
    function GetControlPainterClass: TdxLayoutControlItemControlPainterClass; override;
  end;

  { TcxGridInplaceEditFormContainerPainter }

  TcxGridInplaceEditFormContainerPainter = class
  private
    FCanvas: TcxCanvas;
    FViewInfo: TcxGridInplaceEditFormContainerViewInfo;
  protected
    procedure DrawLayoutGroups; virtual;

    property Canvas: TcxCanvas read FCanvas;
    property ViewInfo: TcxGridInplaceEditFormContainerViewInfo read FViewInfo;
  public
    constructor Create(ACanvas: TcxCanvas; AViewInfo: TcxGridInplaceEditFormContainerViewInfo); virtual;

    procedure Paint; virtual;
  end;

  { TcxGridInplaceEditFormBaseLayoutItemEditViewInfo }

  TcxGridInplaceEditFormBaseLayoutItemEditViewInfo = class(TcxGridCustomLayoutItemEditViewInfo)
  private
    function GetItem: TcxGridInplaceEditFormBaseLayoutItem;inline;
  protected
    function GetDefaultValueHeight: Integer; override;
    function GetRecord: TcxCustomGridRecord; virtual;
    function GetMinValueWidth: Integer; override;

    property Item: TcxGridInplaceEditFormBaseLayoutItem read GetItem;
  end;

  { TcxGridInplaceEditFormLayoutItemEditViewInfo }

  TcxGridInplaceEditFormLayoutItemEditViewInfo = class(TcxGridInplaceEditFormBaseLayoutItemEditViewInfo)
  private
    function GetItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;inline;
  protected
    function GetBorderWidth(ASide: TdxLayoutSide): Integer; override;
    function GetRecord: TcxCustomGridRecord; override;

    property ItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo read GetItemViewInfo;
  end;

  { TcxGridInplaceEditFormBaseLayoutItemViewInfo }

  TcxGridInplaceEditFormBaseLayoutItemViewInfo = class(TcxGridCustomLayoutItemViewInfo)
  private
    function GetItem: TcxGridInplaceEditFormBaseLayoutItem;inline;
  protected
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function GetActuallyVisible: Boolean; override;
    function GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass; override;

    property Item: TcxGridInplaceEditFormBaseLayoutItem read GetItem;
  public
    procedure Calculate(const ABounds: TRect); override;
  end;

  { TcxGridInplaceEditFormLayoutItemViewInfo }

  TcxGridInplaceEditFormLayoutItemViewInfo = class(TcxGridInplaceEditFormBaseLayoutItemViewInfo)
  private
    function GetContainerViewInfo: TcxGridInplaceEditFormContainerViewInfo;inline;
    function GetControlViewInfo: TcxGridInplaceEditFormLayoutItemEditViewInfo;inline;
    function GetGridItemViewInfo: TcxGridInplaceEditFormDataCellViewInfo;inline;
    function GetItem: TcxGridInplaceEditFormLayoutItem;inline;
  protected
    function GetCurrentGridItemViewInfo: TcxGridTableDataCellViewInfo; override;
    function GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass; override;
    function GetPainterClass: TdxCustomLayoutItemPainterClass; override;
  public
    property ContainerViewInfo: TcxGridInplaceEditFormContainerViewInfo read GetContainerViewInfo;
    property ControlViewInfo: TcxGridInplaceEditFormLayoutItemEditViewInfo read GetControlViewInfo;
    property GridItemViewInfo: TcxGridInplaceEditFormDataCellViewInfo read GetGridItemViewInfo;
    property Item: TcxGridInplaceEditFormLayoutItem read GetItem;
  end;

  { TcxGridInplaceEditFormLayoutItemCaptionOptions }

  TcxGridInplaceEditFormLayoutItemCaptionOptions = class(TcxGridCustomLayoutItemCaptionOptions)
  published
    property AlignHorz;
    property AlignVert;
    property Glyph;
    property ImageIndex;
    property Layout default clLeft;
    property Visible;
    property VisibleElements;
    property Width;
  end;

  { TcxGridInplaceEditFormBaseLayoutItem }

  TcxGridInplaceEditFormBaseLayoutItem = class(TcxGridCustomLayoutItem)
  private
    function GetViewInfo: TcxGridInplaceEditFormBaseLayoutItemViewInfo;
  protected
    function GetViewInfoClass: TdxCustomLayoutItemViewInfoClass; override;
  public
    property ViewInfo: TcxGridInplaceEditFormBaseLayoutItemViewInfo read GetViewInfo;
  end;

  { TcxGridInplaceEditFormLayoutItem }

  TcxGridInplaceEditFormLayoutItem = class(TcxGridInplaceEditFormBaseLayoutItem)
  private
    function GetCaptionOptions: TcxGridInplaceEditFormLayoutItemCaptionOptions; inline;
    function GetContainer: TcxGridInplaceEditFormContainer; inline;
    function GetViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;
    procedure SetCaptionOptions(Value: TcxGridInplaceEditFormLayoutItemCaptionOptions); inline;
    procedure SetContainer(Value: TcxGridInplaceEditFormContainer); inline;
  protected
    class function GetCaptionOptionsClass: TdxCustomLayoutItemCaptionOptionsClass; override;
    function GetViewInfoClass: TdxCustomLayoutItemViewInfoClass; override;
  public
    function GetObjectForSelect: TcxGridCustomLayoutItem; override;
    function IsContainerRestoring: Boolean; virtual;

    property Container: TcxGridInplaceEditFormContainer read GetContainer write SetContainer;
    property ViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo read GetViewInfo;
  published
    property CaptionOptions: TcxGridInplaceEditFormLayoutItemCaptionOptions read GetCaptionOptions write SetCaptionOptions;
  end;

  { TcxGridInplaceEditFormGroup }

  TcxGridInplaceEditFormGroup = class(TdxLayoutGroup)
  //TODO check name
  end;

  { TcxGridInplaceEditFormDataCellViewInfo }

  TcxGridInplaceEditFormDataCellViewInfo = class(TcxGridViewLayoutItemDataCellViewInfo)
  private
    function GetLayoutItem: TcxGridInplaceEditFormLayoutItem;
    function GetLayoutItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;
    function GetLayoutItemViewInfoBounds: TRect;
  protected
    function CanAutoHeight: Boolean; override;
    function GetBorders: TcxBorders; override;
    function GetEditBounds: TRect; override;
    function GetEditViewDataBounds: TRect; override;
    function GetLayoutItemViewInfoInstance: TcxGridCustomLayoutItemViewInfo; override;
    function GetRealEditViewDataBounds: TRect; override;

    property LayoutItem: TcxGridInplaceEditFormLayoutItem read GetLayoutItem;
    property LayoutItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo read GetLayoutItemViewInfo;
  public
    property LayoutItemViewInfoBounds: TRect read GetLayoutItemViewInfoBounds;
  end;

  { TcxGridInplaceEditFormContainerGridItemViewInfos }

  TcxGridInplaceEditFormContainerGridItemViewInfos = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxGridInplaceEditFormDataCellViewInfo;
  public
    function FindCellViewInfo(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;

    property Items[Index: Integer]: TcxGridInplaceEditFormDataCellViewInfo read GetItem; default;
  end;

  { TcxGridInplaceEditFormContainerViewInfo }

  TcxGridInplaceEditFormContainerViewInfo = class(TcxGridRecordLayoutContainerViewInfo)
  private
    function GetContainer: TcxGridInplaceEditFormContainer;
    function GetGridView: TcxCustomGridTableView;
    function GetInplaceEditForm: TcxGridInplaceEditForm;
    function GetRootMinHeight: Integer;
  protected
    function GetRecordViewInfoInstance: TcxCustomGridRecordViewInfo; override;
    function GetPainterClass: TcxGridInplaceEditFormContainerPainterClass; virtual;
    function GetViewDataClass(AItem: TdxCustomLayoutItem): TdxCustomLayoutItemViewDataClass; override;

    property GridView: TcxCustomGridTableView read GetGridView;
    property InplaceEditForm: TcxGridInplaceEditForm read GetInplaceEditForm;
  public
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

    procedure Calculate(ARecreateViewData: Boolean); override;

    property Container: TcxGridInplaceEditFormContainer read GetContainer;
    property RootMinHeight: Integer read GetRootMinHeight;
  end;

  { TcxGridInplaceEditFormContainer }

  TcxGridInplaceEditFormContainer = class(TcxGridCustomLayoutContainer)
  private
    FEditor: TcxGridInplaceEditForm;
    FIsDefaultLayoutCreated: Boolean;
    function GetRootMinHeight: Integer;
  protected
    procedure AfterRestoring; override;
    procedure DoInitialize; override;
    function GetDefaultFont: TFont; override;
    function GetDefaultGroupClass: TdxLayoutGroupClass; override;
    function GetItemClass: TcxGridInplaceEditFormLayoutItemClass;
    function GetItemsOwner: TComponent; override;
    function GetViewInfoClass: TdxLayoutContainerViewInfoClass; override;
    function NeedStretchRecordHeight: Boolean; override;
    function NeedStretchRecordWidth: Boolean; override;
    procedure SetDefaultItemName(AItem: TdxCustomLayoutItem); override;
    procedure UpdateRootName; override;

    function CanCreateLayoutItemForGridItem(AItem: TcxCustomGridTableItem): Boolean; virtual;
    function CanSetItemName(AItem: TdxCustomLayoutItem): Boolean; override;
    procedure CheckItemAlignment(AItem: TdxCustomLayoutItem);
    procedure CheckItemsAlignment;
    procedure CheckRootGroupLayoutDirection;
    function CreateLayoutItemForGridItem(AGridItem: TcxCustomGridTableItem): TcxGridInplaceEditFormLayoutItem;
    procedure CreateLayoutItemsForGridItems;
    procedure CopyCustomizationSettings(AContainer: TdxLayoutContainer); virtual;
    procedure Init; virtual;

    procedure CreateColumnDefaultGroup;
    procedure CreateDefaultGroups;
    procedure FixUpItemsOwnership;
    function GetAccessibleDefaultGroup: TdxCustomLayoutGroup;
    function GetValidItemName(const AName: TComponentName; ACheckExisting: Boolean): TComponentName;
    function IsItemVisibleForEditForm(AItem: TcxGridInplaceEditFormLayoutItem): Boolean; virtual;
    procedure PlaceItemsForDefaultLayout; virtual;
    procedure Reset; virtual;

    property InplaceEditForm: TcxGridInplaceEditForm read FEditor;
    property RootMinHeight: Integer read GetRootMinHeight;
  public
    constructor Create(AInplaceEditForm: TcxGridInplaceEditForm); reintroduce; virtual;

    procedure CreateDefaultLayout; virtual;
    procedure DestroyDefaultLayout; virtual;

    procedure CheckItemNames(const AOldName, ANewName: string); override;

    property IsDefaultLayoutCreated: Boolean read FIsDefaultLayoutCreated;
  end;

  { TcxGridInplaceEditFormController }

  TcxGridInplaceEditFormController = class(TcxGridCustomLayoutController)
  private
    FLayoutCustomizationFormHelper: TcxGridLayoutCustomizationFormHelper;
  protected
    function CanCustomize: Boolean; override;
    procedure CheckCustomizationFormBounds(var R: TRect); override;
    function CreateCustomizationForm: TForm; override;
    procedure CustomizationChanged; override;
    procedure DoCreateCustomizationForm; override;
    function GetCustomizationFormBounds: TRect; override;
    function GetCustomizationFormDefaultHeight: Integer; override;
    function GetCustomizationFormDefaultWidth: Integer; override;

    property LayoutCustomizationFormHelper: TcxGridLayoutCustomizationFormHelper read FLayoutCustomizationFormHelper;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
  end;

  { TcxGridInplaceEditForm }

  TcxGridInplaceEditForm = class
  private
    FContainer: TcxGridInplaceEditFormContainer;
    FController: TcxGridInplaceEditFormController;
    FDefaultColumnCount: Integer;
    FMasterRowDblClickAction: TcxGridMasterRowDblClickAction;
    FGridView: TcxCustomGridTableView;
    FEditingRecordIndex: Integer;
    FDefaultStretch: TcxGridInplaceEditFormStretch;
    FItemHotTrack: Boolean;
    FUseDefaultLayout: Boolean;

    procedure CreateContainer;
    procedure CreateController;
    procedure DestroyContainer;
    procedure DestroyController;

    procedure ContainerChangedHandler(Sender: TObject);
    function GetDefaultColumnCount: Integer;
    function GetItemHotTrack: Boolean;
    function GetMasterRowDblClickAction: TcxGridMasterRowDblClickAction;
    function GetEditingRecordIndex: Integer;
    procedure SetDefaultColumnCount(AValue: Integer);
    procedure SetItemHotTrack(AValue: Boolean);
    procedure SetMasterRowDblClickAction(AValue: TcxGridMasterRowDblClickAction);
    procedure SetEditingRecordIndex(AValue: Integer);
    procedure SetDefaultStretch(AValue: TcxGridInplaceEditFormStretch);
    procedure SetUseDefaultLayout(AValue: Boolean);
  protected
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    procedure AdjustEditingItemOnVisibilityChange; virtual;
    procedure AfterRestoring; virtual;
    procedure AssignStructure(AInplaceEditForm: TcxGridInplaceEditForm);
    procedure BeforeRestoring; virtual;
    function CanCreateLayoutItem: Boolean; virtual;
    function CanShowCustomizationForm: Boolean; virtual;
    procedure Changed(AHardUpdate: Boolean = False); virtual; abstract;
    procedure CheckContainerName(const AOldName, ANewName: string); virtual;
    function CreateLayoutItemForGridItem(AGridItem: TcxCustomGridTableItem): TcxGridInplaceEditFormLayoutItem; virtual;
    function IsAssigningOptions: Boolean; virtual; abstract;
    function GetLayoutLookAndFeel: TdxCustomLayoutLookAndFeel; virtual;
    function GetVisible: Boolean; virtual;
    procedure Init; virtual;
    function IsInplaceEditFormMode: Boolean; virtual; abstract;
    procedure StoreChildren(Proc: TGetChildProc); virtual;
    procedure ValidateFocusedItem; virtual;

    function CloseOnRecordInserting: Boolean; virtual;
    procedure CloseOnFocusedRecordChanging(var AFocusingRecord: TcxCustomGridRecord); virtual;
    function CloseQuery(ARaiseAbortOnCancel: Boolean = True): TModalResult;
    procedure InvalidateUpdateButton;
    procedure ResetEditingRecordIndex; virtual;

    function NeedStretchRecordHeight: Boolean;
    function NeedStretchRecordWidth: Boolean;

    function GetContainerClass: TcxGridInplaceEditFormContainerClass; virtual;
    function GetControllerClass: TcxGridInplaceEditFormControllerClass; virtual;

    property Container: TcxGridInplaceEditFormContainer read FContainer;
    property Controller: TcxGridInplaceEditFormController read FController;
    property DefaultColumnCount: Integer read GetDefaultColumnCount write SetDefaultColumnCount;
    property DefaultStretch: TcxGridInplaceEditFormStretch read FDefaultStretch write SetDefaultStretch;
    property EditingRecordIndex: Integer read GetEditingRecordIndex
      write SetEditingRecordIndex;
    property ItemHotTrack: Boolean read GetItemHotTrack write SetItemHotTrack;
    property LayoutLookAndFeel: TdxCustomLayoutLookAndFeel read GetLayoutLookAndFeel;
    property MasterRowDblClickAction: TcxGridMasterRowDblClickAction read GetMasterRowDblClickAction
      write SetMasterRowDblClickAction;
    property UseDefaultLayout: Boolean read FUseDefaultLayout write SetUseDefaultLayout;
  public
    constructor Create(AGridView: TcxCustomGridTableView); reintroduce; virtual;
    destructor Destroy; override;

    function Close(ARaiseAbortOnCancel: Boolean = True): Boolean; virtual;
    procedure CopyCustomizationSetting(AContainer: TdxLayoutContainer); virtual;
    function IsUpdateButtonEnabled: Boolean; virtual;
    function IsUpdateButtonVisible: Boolean; virtual;
    function MinHeight: Integer;
    procedure ShowCustomizationForm; virtual;

    procedure CancelExecute;
    procedure UpdateExecute;

    property GridView: TcxCustomGridTableView read FGridView;
    property Visible: Boolean read GetVisible;
  end;

implementation

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  RTLConsts, cxGeometry, cxContainer, cxGridTableViewInplaceEditFormCustomizationForm, cxGridTableView,
  Dialogs, cxGridRows, cxGridStrs;

const
  scxGridInplaceEditFormResetCustomSettingLayoutQuery = 'This operation will revert the Edit Form layout settings to the default settings. Do you wish to continue?';

type
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TcxControlAccess = class(TcxControl);
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TcxCustomGridTableViewAccess = class(TcxCustomGridTableView);

{ TcxGridInplaceEditFormLayoutLookAndFeel }

constructor TcxGridInplaceEditFormLayoutLookAndFeel.Create(AGridView: TcxCustomGridTableView);
begin
  inherited Create(AGridView);
  LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
  Offsets.RootItemsAreaOffsetVert := 10;
  Offsets.RootItemsAreaOffsetHorz := 13;
end;

{ TcxGridInplaceEditFormLayoutItemEditPainter }

procedure TcxGridInplaceEditFormLayoutItemEditPainter.DrawEdit(ACanvas: TcxCanvas);
begin
// do nothing
end;

function TcxGridInplaceEditFormLayoutItemEditPainter.GetViewInfo: TcxGridInplaceEditFormLayoutItemEditViewInfo;
begin
  Result := (inherited ViewInfo as TcxGridInplaceEditFormLayoutItemEditViewInfo);
end;

{ TcxGridInplaceEditFormLayoutItemPainter }

function TcxGridInplaceEditFormLayoutItemPainter.GetControlPainterClass: TdxLayoutControlItemControlPainterClass;
begin
  Result := TcxGridInplaceEditFormLayoutItemEditPainter;
end;

{ TcxGridInplaceEditFormContainerPainter }

constructor TcxGridInplaceEditFormContainerPainter.Create(ACanvas: TcxCanvas;
  AViewInfo: TcxGridInplaceEditFormContainerViewInfo);
begin
  inherited Create;
  FCanvas := ACanvas;
  FViewInfo := AViewInfo;
end;

procedure TcxGridInplaceEditFormContainerPainter.DrawLayoutGroups;
var
  APainter: TdxCustomLayoutItemPainter;
  AItemsViewInfo: TdxCustomLayoutItemViewInfoAccess;
begin
  AItemsViewInfo := TdxCustomLayoutItemViewInfoAccess(ViewInfo.ItemsViewInfo);
  APainter := AItemsViewInfo.GetPainterClass.Create(ViewInfo.ItemsViewInfo);
  try
    APainter.Paint(Canvas);
  finally
    APainter.Free;
  end;
end;

procedure TcxGridInplaceEditFormContainerPainter.Paint;
begin
  DrawLayoutGroups;
end;

{ TcxGridInplaceEditFormBaseLayoutItemEditViewInfo }

function TcxGridInplaceEditFormBaseLayoutItemEditViewInfo.GetDefaultValueHeight: Integer;
var
  AParams: TcxViewParams;
begin
  GridView.Styles.GetContentParams(GetRecord, Item.GridViewItem, AParams);
  Result := Item.GridViewItem.CalculateDefaultCellHeight(cxScreenCanvas, AParams.Font);
end;

function TcxGridInplaceEditFormBaseLayoutItemEditViewInfo.GetItem: TcxGridInplaceEditFormBaseLayoutItem;
begin
  Result := TcxGridInplaceEditFormBaseLayoutItem(inherited Item);
end;

function TcxGridInplaceEditFormBaseLayoutItemEditViewInfo.GetRecord: TcxCustomGridRecord;
begin
  Result := nil;
end;

function TcxGridInplaceEditFormBaseLayoutItemEditViewInfo.GetMinValueWidth: Integer;
begin
  Result := cxGridInplaceEditFormLayoutItemEditDefaultMinValueWidth;
end;

{ TcxGridInplaceEditFormLayoutItemEditViewInfo }

function TcxGridInplaceEditFormLayoutItemEditViewInfo.GetBorderWidth(ASide: TdxLayoutSide): Integer;

  function LayoutSideToGridBorder: TcxBorder;
  begin
    case ASide of
      sdLeft:
        Result := bLeft;
      sdRight:
        Result := bRight;
      sdTop:
        Result := bTop;
    else
      Result := bBottom;
    end;
  end;

begin
  if Assigned(ItemViewInfo.GridItemViewInfo) then
    Result := ItemViewInfo.GridItemViewInfo.BorderSize[LayoutSideToGridBorder]
  else
    Result := inherited GetBorderWidth(ASide);
end;

function TcxGridInplaceEditFormLayoutItemEditViewInfo.GetRecord: TcxCustomGridRecord;
begin
  Result := GridView.ViewData.EditingRecord;
end;

function TcxGridInplaceEditFormLayoutItemEditViewInfo.GetItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;
begin
  Result := TcxGridInplaceEditFormLayoutItemViewInfo(inherited ItemViewInfo);
end;

{ TcxGridInplaceEditFormBaseLayoutItemViewInfo }

procedure TcxGridInplaceEditFormBaseLayoutItemViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  if Assigned(GridItemViewInfo) then
    GridItemViewInfo.Calculate(ABounds);
end;

procedure TcxGridInplaceEditFormBaseLayoutItemViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  if Assigned(GridItemViewInfo) then
    GridItemViewInfo.Calculate(Bounds);
end;

function TcxGridInplaceEditFormBaseLayoutItemViewInfo.GetActuallyVisible: Boolean;
begin
  Result := Assigned(Item) and
    Assigned(Item.GridViewItem) and
    inherited GetActuallyVisible;
end;

function TcxGridInplaceEditFormBaseLayoutItemViewInfo.GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass;
begin
  Result := TcxGridInplaceEditFormBaseLayoutItemEditViewInfo;
end;

function TcxGridInplaceEditFormBaseLayoutItemViewInfo.GetItem: TcxGridInplaceEditFormBaseLayoutItem;
begin
  Result:= TcxGridInplaceEditFormBaseLayoutItem(inherited Item);
end;

{ TcxGridInplaceEditFormLayoutItemViewInfo }

function TcxGridInplaceEditFormLayoutItemViewInfo.GetContainerViewInfo: TcxGridInplaceEditFormContainerViewInfo;
begin
  Result := TcxGridInplaceEditFormContainerViewInfo(inherited ContainerViewInfo);
end;

function TcxGridInplaceEditFormLayoutItemViewInfo.GetControlViewInfo: TcxGridInplaceEditFormLayoutItemEditViewInfo;
begin
  Result := TcxGridInplaceEditFormLayoutItemEditViewInfo(inherited ControlViewInfo);
end;

function TcxGridInplaceEditFormLayoutItemViewInfo.GetGridItemViewInfo: TcxGridInplaceEditFormDataCellViewInfo;
begin
  Result := TcxGridInplaceEditFormDataCellViewInfo(inherited GridItemViewInfo);
end;

function TcxGridInplaceEditFormLayoutItemViewInfo.GetItem: TcxGridInplaceEditFormLayoutItem;
begin
  Result := TcxGridInplaceEditFormLayoutItem(inherited Item);
end;

function TcxGridInplaceEditFormLayoutItemViewInfo.GetCurrentGridItemViewInfo: TcxGridTableDataCellViewInfo;
begin
  Result := ContainerViewInfo.GetGridItemViewInfo(Self);
end;

function TcxGridInplaceEditFormLayoutItemViewInfo.GetControlViewInfoClass: TdxLayoutControlItemControlViewInfoClass;
begin
  Result := TcxGridInplaceEditFormLayoutItemEditViewInfo;
end;

function TcxGridInplaceEditFormLayoutItemViewInfo.GetPainterClass: TdxCustomLayoutItemPainterClass;
begin
  Result := TcxGridInplaceEditFormLayoutItemPainter;
end;

{ TcxGridInplaceEditFormBaseLayoutItem }

function TcxGridInplaceEditFormBaseLayoutItem.GetViewInfo: TcxGridInplaceEditFormBaseLayoutItemViewInfo;
begin
  Result := TcxGridInplaceEditFormBaseLayoutItemViewInfo(inherited ViewInfo);
end;

function TcxGridInplaceEditFormBaseLayoutItem.GetViewInfoClass: TdxCustomLayoutItemViewInfoClass;
begin
  Result := TcxGridInplaceEditFormBaseLayoutItemViewInfo;
end;

{ TcxGridInplaceEditFormLayoutItem }

function TcxGridInplaceEditFormLayoutItem.GetObjectForSelect: TcxGridCustomLayoutItem;
begin
  Result := inherited GetObjectForSelect;
end;

function TcxGridInplaceEditFormLayoutItem.IsContainerRestoring: Boolean;
begin
  Result := Container.IsRestoring;
end;

class function TcxGridInplaceEditFormLayoutItem.GetCaptionOptionsClass: TdxCustomLayoutItemCaptionOptionsClass;
begin
  Result := TcxGridInplaceEditFormLayoutItemCaptionOptions;
end;

function TcxGridInplaceEditFormLayoutItem.GetViewInfoClass: TdxCustomLayoutItemViewInfoClass;
begin
  Result := TcxGridInplaceEditFormLayoutItemViewInfo;
end;

function TcxGridInplaceEditFormLayoutItem.GetCaptionOptions: TcxGridInplaceEditFormLayoutItemCaptionOptions;
begin
  Result := TcxGridInplaceEditFormLayoutItemCaptionOptions(inherited CaptionOptions);
end;

function TcxGridInplaceEditFormLayoutItem.GetContainer: TcxGridInplaceEditFormContainer;
begin
  Result := TcxGridInplaceEditFormContainer(inherited Container);
end;

function TcxGridInplaceEditFormLayoutItem.GetViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;
begin
  Result := TcxGridInplaceEditFormLayoutItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridInplaceEditFormLayoutItem.SetCaptionOptions(Value: TcxGridInplaceEditFormLayoutItemCaptionOptions);
begin
  inherited CaptionOptions := Value;
end;

procedure TcxGridInplaceEditFormLayoutItem.SetContainer(Value: TcxGridInplaceEditFormContainer);
begin
  inherited Container := Value;
end;

{ TcxGridInplaceEditFormDataCellViewInfo }

function TcxGridInplaceEditFormDataCellViewInfo.CanAutoHeight: Boolean;
begin
  Result := False;
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetBorders: TcxBorders;
begin
  Result := [bLeft, bRight, bTop, bBottom];
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetEditBounds: TRect;
var
  ADataRow: TcxGridDataRowViewInfo;
begin
  Result := inherited GetEditBounds;
  ADataRow := TcxGridDataRowViewInfo(RecordViewInfo);
  Result := ADataRow.InplaceEditFormAreaViewInfo.GetBoundsForEdit(Result);
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetEditViewDataBounds: TRect;
begin
  if LayoutItemViewInfo <> nil then
    Result := LayoutItemViewInfo.ControlViewInfo.Bounds
  else
    Result := inherited GetEditViewDataBounds;
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetLayoutItemViewInfoInstance: TcxGridCustomLayoutItemViewInfo;
begin
  if LayoutItem <> nil then
    Result := LayoutItem.ViewInfo
  else
    Result := nil;
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetLayoutItem: TcxGridInplaceEditFormLayoutItem;
begin
  if Item is TcxGridColumn then
    Result := TcxGridColumn(Item).LayoutItem
  else
    Result := nil;
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetLayoutItemViewInfo: TcxGridInplaceEditFormLayoutItemViewInfo;
begin
  Result := TcxGridInplaceEditFormLayoutItemViewInfo(inherited LayoutItemViewInfo);
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetLayoutItemViewInfoBounds: TRect;
begin
  if LayoutItemViewInfo <> nil then
    Result := LayoutItemViewInfo.Bounds
  else
    Result := cxEmptyRect;
end;

function TcxGridInplaceEditFormDataCellViewInfo.GetRealEditViewDataBounds: TRect;
begin
  Result := GetEditViewDataBounds;
end;

{ TcxGridInplaceEditFormContainerGridItemViewInfos }

function TcxGridInplaceEditFormContainerGridItemViewInfos.FindCellViewInfo(
  AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Item = AItem then
    begin
      Result := Items[I];
      Exit;
    end;
end;

function TcxGridInplaceEditFormContainerGridItemViewInfos.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I].LayoutItemViewInfo <> nil) and
      Items[I].LayoutItemViewInfo.ActuallyVisible then
      Result := Items[I].GetHitTest(P);
    if Result <> nil then
      Exit;
  end;
end;

function TcxGridInplaceEditFormContainerGridItemViewInfos.GetItem(
  Index: Integer): TcxGridInplaceEditFormDataCellViewInfo;
begin
  Result := TcxGridInplaceEditFormDataCellViewInfo(inherited Items[Index]);
end;

{ TcxGridInplaceEditFormContainerViewInfo }

procedure TcxGridInplaceEditFormContainerViewInfo.Paint(ACanvas: TcxCanvas);
var
  APainter: TcxGridInplaceEditFormContainerPainter;
begin
  APainter := GetPainterClass.Create(ACanvas, Self);
  try
    APainter.Paint;
  finally
    APainter.Free;
  end;
end;

procedure TcxGridInplaceEditFormContainerViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  MouseMove(Shift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

function TcxGridInplaceEditFormContainerViewInfo.GetRecordViewInfoInstance: TcxCustomGridRecordViewInfo;
var
  ARecord: TcxCustomGridRecord;
begin
  ARecord := GridView.ViewData.GetRecordByRecordIndex(InplaceEditForm.EditingRecordIndex);
  if (ARecord <> nil) then
  begin
    Result := ARecord.ViewInfo;
    if (Result <> nil) and not Result.Calculated then
      Result := nil;
  end
  else
    Result := inherited GetRecordViewInfoInstance;
end;

function TcxGridInplaceEditFormContainerViewInfo.GetPainterClass: TcxGridInplaceEditFormContainerPainterClass;
begin
  Result := TcxGridInplaceEditFormContainerPainter;
end;

function TcxGridInplaceEditFormContainerViewInfo.GetViewDataClass(AItem: TdxCustomLayoutItem): TdxCustomLayoutItemViewDataClass;
begin
  if AItem is TdxCustomLayoutGroup then
    Result := TdxLayoutGroupViewData
  else
    Result := inherited GetViewDataClass(AItem);
end;

procedure TcxGridInplaceEditFormContainerViewInfo.Calculate(ARecreateViewData: Boolean);
begin
  if InplaceEditForm.GridView.ComponentState * [csReading, csLoading] = [] then
    inherited;
end;

function TcxGridInplaceEditFormContainerViewInfo.GetContainer: TcxGridInplaceEditFormContainer;
begin
  Result := TcxGridInplaceEditFormContainer(inherited Container);
end;

function TcxGridInplaceEditFormContainerViewInfo.GetGridView: TcxCustomGridTableView;
begin
  Result := InplaceEditForm.GridView;
end;

function TcxGridInplaceEditFormContainerViewInfo.GetInplaceEditForm: TcxGridInplaceEditForm;
begin
  Result := Container.InplaceEditForm;
end;

function TcxGridInplaceEditFormContainerViewInfo.GetRootMinHeight: Integer;
begin
  Result := Container.RootMinHeight;
end;

{ TcxGridInplaceEditFormContainer }

constructor TcxGridInplaceEditFormContainer.Create(AInplaceEditForm: TcxGridInplaceEditForm);
begin
  inherited Create(AInplaceEditForm.GridView);
  FEditor := AInplaceEditForm;
end;

procedure TcxGridInplaceEditFormContainer.CopyCustomizationSettings(AContainer: TdxLayoutContainer);
begin
  GridView.BeginUpdate;
  try
    CustomizationHelper.CopyStructure(AContainer);
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridInplaceEditFormContainer.Reset;
begin
  BeginUpdate;
  try
    DestroyItems;
    CreateItems;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridInplaceEditFormContainer.AfterRestoring;
var
  I: Integer;
  AName: string;
  AItem: TdxCustomLayoutItem;
  AGridViewItem: TcxCustomGridTableItem;
  AGridView: TcxCustomGridTableViewAccess;
  AGridLayoutItem: TcxGridInplaceEditFormLayoutItem;
begin
  BeginUpdate;
  try
    for I := AbsoluteItemCount - 1 downto 0 do
    begin
      AItem := AbsoluteItems[I];
      if AItem is TcxGridInplaceEditFormLayoutItem then
      begin
        AGridLayoutItem := TcxGridInplaceEditFormLayoutItem(AItem);
        AGridView := TcxCustomGridTableViewAccess(GridView);
        AName := AGridLayoutItem.LoadedGridViewItemName;
        AGridViewItem := AGridView.FindItemByObjectName(AName);
        if AGridViewItem <> nil then
        begin
          AGridLayoutItem.GridViewItem := AGridViewItem;
          if AGridLayoutItem.CaptionOptions.IsCaptionAssigned then
            AGridViewItem.Caption := AGridLayoutItem.CaptionOptions.GridItemCaption;
        end
        else
          if AGridLayoutItem.GridViewItem = nil then
            FreeAndNil(AGridLayoutItem);
      end;
    end;
    inherited AfterRestoring;
    if InplaceEditForm.UseDefaultLayout then
    begin
      DestroyDefaultLayout;
      if InplaceEditForm.Visible then
        CreateDefaultLayout;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridInplaceEditFormContainer.DoInitialize;
begin
  inherited DoInitialize;
  MenuItems := MenuItems - [cfmiExpandButton];
end;

function TcxGridInplaceEditFormContainer.GetDefaultFont: TFont;
begin
  if GridView.Control <> nil then
    Result := TcxGrid(GridView.Control).Font
  else
    Result := TcxControlAccess(GridView.Site).Font;
end;

function TcxGridInplaceEditFormContainer.GetDefaultGroupClass: TdxLayoutGroupClass;
begin
  Result := TcxGridInplaceEditFormGroup;
end;

function TcxGridInplaceEditFormContainer.GetItemClass: TcxGridInplaceEditFormLayoutItemClass;
begin
  Result := TcxGridInplaceEditFormLayoutItem;
end;

function TcxGridInplaceEditFormContainer.GetItemsOwner: TComponent;
begin
  Result := Self;
end;

function TcxGridInplaceEditFormContainer.GetViewInfoClass: TdxLayoutContainerViewInfoClass;
begin
  Result := TcxGridInplaceEditFormContainerViewInfo;
end;

procedure TcxGridInplaceEditFormContainer.CheckItemAlignment(AItem: TdxCustomLayoutItem);
begin
  if (AItem.Parent <> nil) then
  begin
    AItem.AlignHorz := Root.AlignHorz;
    AItem.AlignVert := Root.AlignVert;
  end;
end;

procedure TcxGridInplaceEditFormContainer.CheckItemsAlignment;
var
  I: Integer;
begin
  for I := 0 to AbsoluteItemCount - 1 do
    CheckItemAlignment(AbsoluteItems[I]);
end;

procedure TcxGridInplaceEditFormContainer.CheckRootGroupLayoutDirection;
begin
  if InplaceEditForm.DefaultColumnCount > 1 then
    Root.LayoutDirection := ldHorizontal
  else
    Root.LayoutDirection := ldVertical;
end;

function TcxGridInplaceEditFormContainer.CreateLayoutItemForGridItem(AGridItem: TcxCustomGridTableItem): TcxGridInplaceEditFormLayoutItem;
begin
  Result := TcxGridInplaceEditFormLayoutItem(inherited CreateItem(GetItemClass, nil));
  Result.GridViewItem := AGridItem;
end;

procedure TcxGridInplaceEditFormContainer.CreateLayoutItemsForGridItems;
var
  I: Integer;
  ACustomItem: TcxCustomGridTableItem;
  AItem: TcxGridColumn;
begin
  if GridView <> nil then
  begin
    for I := 0 to GridView.ItemCount - 1 do
    begin
      ACustomItem := GridView.Items[I];
      if not (ACustomItem is TcxGridColumn) then
        Continue;
      AItem := TcxGridColumn(ACustomItem);
      if CanCreateLayoutItemForGridItem(AItem) then
        AItem.LayoutItem := CreateLayoutItemForGridItem(AItem);
      if AItem.IsPreview then
        AItem.LayoutItem := nil;
    end;
  end;
end;

procedure TcxGridInplaceEditFormContainer.Init;
begin
  if (Root <> nil) and not InplaceEditForm.UseDefaultLayout then
  begin
    CreateLayoutItemsForGridItems;
    CheckIndexes;
    LayoutChanged;
  end;
end;

function TcxGridInplaceEditFormContainer.CanCreateLayoutItemForGridItem(
  AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := False;
end;

function TcxGridInplaceEditFormContainer.CanSetItemName(AItem: TdxCustomLayoutItem): Boolean;
begin
  Result := (ItemsParentComponent <> nil) and not (csAncestor in AItem.ComponentState) and SelectionHelper.CanModify;
end;

function TcxGridInplaceEditFormContainer.NeedStretchRecordHeight: Boolean;
begin
  Result := InplaceEditForm.NeedStretchRecordHeight;
end;

function TcxGridInplaceEditFormContainer.NeedStretchRecordWidth: Boolean;
begin
  Result := InplaceEditForm.NeedStretchRecordWidth;
end;

procedure TcxGridInplaceEditFormContainer.CreateColumnDefaultGroup;
var
  AGroup: TdxCustomLayoutGroup;
begin
  AGroup := GetAutoCreatedGroup;
  AGroup.Parent := Root;
  AGroup.LayoutDirection := ldVertical;
  CheckItemAlignment(AGroup);
end;

procedure TcxGridInplaceEditFormContainer.CreateDefaultGroups;
var
  I: Integer;
begin
  for I := 0 to InplaceEditForm.DefaultColumnCount - 1 do
    CreateColumnDefaultGroup;
end;

procedure TcxGridInplaceEditFormContainer.CreateDefaultLayout;
begin
  BeginUpdate;
  try
    FIsDefaultLayoutCreated := True;
    UpdateRootOptions;
    CheckRootGroupLayoutDirection;
    CreateDefaultGroups;
    CreateLayoutItemsForGridItems;
    PlaceItemsForDefaultLayout;
    CheckItemsAlignment;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridInplaceEditFormContainer.DestroyDefaultLayout;
begin
  Reset;
  FIsDefaultLayoutCreated := False;
end;

function TcxGridInplaceEditFormContainer.GetAccessibleDefaultGroup: TdxCustomLayoutGroup;
var
  I: Integer;
  AGroup: TdxCustomLayoutGroup;
begin
  Result := nil;
  for I := 0 to AbsoluteItemCount - 1 do
    if AbsoluteItems[I] is TdxCustomLayoutGroup then
    begin
      AGroup := TdxCustomLayoutGroup(AbsoluteItems[I]);
      if (Result = nil) or (AGroup.Count < Result.Count) then
        Result := AGroup;
    end;
end;

function TcxGridInplaceEditFormContainer.IsItemVisibleForEditForm(AItem: TcxGridInplaceEditFormLayoutItem): Boolean;
begin
  Result := AItem.Visible;
end;

procedure TcxGridInplaceEditFormContainer.PlaceItemsForDefaultLayout;
var
  I: Integer;
  AItem: TcxGridInplaceEditFormLayoutItem;
begin
  for I := 0 to AbsoluteItemCount - 1 do
    if AbsoluteItems[I] is TcxGridInplaceEditFormLayoutItem then
    begin
      AItem := TcxGridInplaceEditFormLayoutItem(AbsoluteItems[I]);
      if IsItemVisibleForEditForm(AItem) then
        AItem.Parent := GetAccessibleDefaultGroup;
    end;
end;

function TcxGridInplaceEditFormContainer.GetRootMinHeight: Integer;
begin
  if Root <> nil then
    Result := Root.ViewInfo.MinHeight
  else
    Result := 0;
end;


procedure TcxGridInplaceEditFormContainer.SetDefaultItemName(AItem: TdxCustomLayoutItem);
begin
  if not GridView.IsLoading then
    AItem.Name := GetValidItemName(TdxCustomLayoutItemAccess(AItem).GetBaseName, False);
end;

procedure TcxGridInplaceEditFormContainer.UpdateRootName;
begin
  Root.Name := GetValidItemName(ItemsParentComponent.Name + 'RootGroup', True);
end;

function TcxGridInplaceEditFormContainer.GetValidItemName(const AName: TComponentName; ACheckExisting: Boolean): TComponentName;
var
  I: Integer;

  function GetNextName: string;
  begin
    Result := AName + IntToStr(I);
    Inc(I);
  end;

  function IsValidName(const AName: TComponentName): Boolean;
  begin
    Result := (FindComponent(AName) = nil) and ((GridView.Owner = nil) or (GridView.Owner.FindComponent(AName) = nil));
  end;

begin
  I := 1;
  if ACheckExisting then
    Result := AName
  else
    Result := GetNextName;
  while not IsValidName(Result) do
    Result := GetNextName;
end;

procedure TcxGridInplaceEditFormContainer.CheckItemNames(const AOldName, ANewName: string);

  procedure RenameComponent(AComponent: TComponent; ANewName: TComponentName;
    const AOldName: TComponentName; AValidate: Boolean);
  var
    AComponentName, ANamePrefix: TComponentName;
  begin
    AComponentName := AComponent.Name;
    if Length(AComponentName) > Length(AOldName) then
    begin
      ANamePrefix := Copy(AComponentName, 1, Length(AOldName));
      if CompareText(AOldName, ANamePrefix) = 0 then
      begin
        Delete(AComponentName, 1, Length(AOldName));
        Insert(ANewName, AComponentName, 1);
        if AValidate then
          AComponentName := GetValidItemName(AComponentName, True);
        try
          AComponent.Name := AComponentName;
        except
          on EComponentError do ;
        end;
      end;
    end;
  end;

  procedure RenameComponents(ACaller: TComponent; const ANewName, AOldName: TComponentName;
    AComponentCount: Integer; AGetComponent: TcxGetComponent);
  var
    I: Integer;
    AComponent: TComponent;
  begin
    for I := 0 to AComponentCount - 1 do
    begin
      AComponent := AGetComponent(ACaller, I);
      RenameComponent(AComponent, ANewName, AOldName, False);
    end;
  end;

  function GetItem(ACaller: TComponent; Index: Integer): TComponent;
  begin
    Result := TdxLayoutContainer(ACaller).AbsoluteItems[Index];
  end;

  function GetAlignmentConstraints(ACaller: TComponent; Index: Integer): TComponent;
  begin
    Result := TdxLayoutContainer(ACaller).AlignmentConstraints[Index];
  end;

begin
  if CanSetItemName(Root) and not (csAncestor in Owner.ComponentState) then
  begin
    UpdateRootName;
    RenameComponents(Self, ANewName, AOldName, AbsoluteItemCount, @GetItem);
    RenameComponents(Self, ANewName, AOldName, AlignmentConstraintCount, @GetAlignmentConstraints);
    CustomizeFormPostUpdate([cfutCaption]);
  end;
end;

procedure TcxGridInplaceEditFormContainer.FixUpItemsOwnership;
var
  I: Integer;
  ALayoutItem: TdxCustomLayoutItem;
  AGridItem: TcxCustomGridTableItem;
begin
  if AbsoluteItemCount = 0 then
    Exit;
  BeginUpdate;
  try
    for I := 0 to AbsoluteItemCount - 1 do
    begin
      ALayoutItem := AbsoluteItems[I];
      if ALayoutItem.Owner = Self then
        Continue;
      if ALayoutItem is TcxGridInplaceEditFormLayoutItem then
        AGridItem := TcxGridInplaceEditFormLayoutItem(ALayoutItem).GridViewItem
      else
        AGridItem := nil;
      InsertComponent(ALayoutItem);
      if AGridItem is TcxGridColumn then
        TcxGridColumn(AGridItem).LayoutItem := TcxGridInplaceEditFormLayoutItem(ALayoutItem);
    end;
  finally
    EndUpdate;
  end;
end;

{ TcxGridInplaceEditFormController }

constructor TcxGridInplaceEditFormController.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FLayoutCustomizationFormHelper := TcxGridLayoutCustomizationFormHelper.Create(Self);
end;

destructor TcxGridInplaceEditFormController.Destroy;
begin
  FreeAndNil(FLayoutCustomizationFormHelper);
  inherited Destroy;
end;

function TcxGridInplaceEditFormController.CanCustomize: Boolean;
begin
  Result := True;
end;

procedure TcxGridInplaceEditFormController.CheckCustomizationFormBounds(var R: TRect);
begin
  LayoutCustomizationFormHelper.CheckCustomizationFormBounds(R);
end;

function TcxGridInplaceEditFormController.CreateCustomizationForm: TForm;
begin
  Result := TcxGridTableViewInplaceEditFormCustomizationForm.Create(nil);
end;

procedure TcxGridInplaceEditFormController.CustomizationChanged;
begin
  LayoutCustomizationFormHelper.CustomizationChanged;
end;

procedure TcxGridInplaceEditFormController.DoCreateCustomizationForm;
begin
  inherited DoCreateCustomizationForm;
  LayoutCustomizationFormHelper.InitializeCustomizationForm;
end;

function TcxGridInplaceEditFormController.GetCustomizationFormBounds: TRect;
begin
  Result := LayoutCustomizationFormHelper.GetCustomizationFormBounds;
end;

function TcxGridInplaceEditFormController.GetCustomizationFormDefaultHeight: Integer;
begin
  Result := cxGridLayoutCustomizationFormDefaultHeight;
end;

function TcxGridInplaceEditFormController.GetCustomizationFormDefaultWidth: Integer;
begin
  Result := cxGridLayoutCustomizationFormDefaultWidth;
end;

{ TcxGridInplaceEditForm }

constructor TcxGridInplaceEditForm.Create(AGridView: TcxCustomGridTableView);
begin
  inherited Create;
  FGridView := AGridView;
  CreateSubClasses;
  FEditingRecordIndex := cxGridInplaceEditFormEditingRecordIndexNone;
  FUseDefaultLayout := True;
  DefaultColumnCount := cxGridInplaceEditFormDefaultColumnCount;
end;

destructor TcxGridInplaceEditForm.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

function TcxGridInplaceEditForm.Close(ARaiseAbortOnCancel: Boolean = True): Boolean;
begin
  if GridView.DataController.IsEditing and (dceModified in GridView.DataController.EditState) then
    case CloseQuery(ARaiseAbortOnCancel) of
      mrYes:
        GridView.DataController.Post(True);
      mrNo:
        GridView.DataController.Cancel;
    end
  else
  begin
    ResetEditingRecordIndex;
    GridView.DataController.Cancel;
  end;
  Result := EditingRecordIndex = cxGridInplaceEditFormEditingRecordIndexNone;
end;

procedure TcxGridInplaceEditForm.CopyCustomizationSetting(AContainer: TdxLayoutContainer);
begin
  UseDefaultLayout := False;
  Container.CopyCustomizationSettings(AContainer);
end;

function TcxGridInplaceEditForm.IsUpdateButtonEnabled: Boolean;
begin
  Result := GridView.Controller.EditingController.IsRecordModified;
end;

function TcxGridInplaceEditForm.IsUpdateButtonVisible: Boolean;
begin
  Result := GridView.OptionsData.Editing;
end;

function TcxGridInplaceEditForm.MinHeight: Integer;
begin
  Result := Container.ViewInfo.ItemsViewInfo.MinHeight;
end;

procedure TcxGridInplaceEditForm.ShowCustomizationForm;
begin
  if CanShowCustomizationForm then
    Controller.Customization := True;
end;

procedure TcxGridInplaceEditForm.CancelExecute;
begin
  if GridView.DataController.IsEditing then
    GridView.DataController.Cancel
  else
    Close;
end;

procedure TcxGridInplaceEditForm.UpdateExecute;
begin
  if GridView.DataController.IsEditing then
    GridView.DataController.Post(True)
  else
    Close;
end;

procedure TcxGridInplaceEditForm.CreateSubClasses;
begin
  CreateContainer;
  CreateController;
end;

procedure TcxGridInplaceEditForm.DestroySubClasses;
begin
  DestroyController;
  DestroyContainer;
end;

procedure TcxGridInplaceEditForm.AdjustEditingItemOnVisibilityChange;
begin
  if Visible then
    GridView.Controller.EditingItem := GridView.Controller.FocusedItem
  else
    GridView.Controller.EditingController.HideEdit(False);
end;

procedure TcxGridInplaceEditForm.AfterRestoring;
begin
  Container.IsRestoring := False;
  Container.EndUpdate;
end;

procedure TcxGridInplaceEditForm.AssignStructure(AInplaceEditForm: TcxGridInplaceEditForm);
begin
  Container.BeginUpdate;
  try
    LayoutLookAndFeel.Assign(AInplaceEditForm.LayoutLookAndFeel);
    if not AInplaceEditForm.UseDefaultLayout then
      Container.CopyCustomizationSettings(AInplaceEditForm.Container);
  finally
    Container.EndUpdate;
  end;
end;

procedure TcxGridInplaceEditForm.BeforeRestoring;
begin
  Container.BeginUpdate;
  Container.IsRestoring := True;
end;

function TcxGridInplaceEditForm.CanCreateLayoutItem: Boolean;
begin
  Result := not UseDefaultLayout or Container.IsDefaultLayoutCreated;
end;

function TcxGridInplaceEditForm.CanShowCustomizationForm: Boolean;
begin
  Result := True;
end;

procedure TcxGridInplaceEditForm.CheckContainerName(const AOldName, ANewName: string);
begin
  Container.CheckItemNames(AOldName, ANewName);
end;

function TcxGridInplaceEditForm.CreateLayoutItemForGridItem(AGridItem: TcxCustomGridTableItem): TcxGridInplaceEditFormLayoutItem;
begin
  Result := Container.CreateLayoutItemForGridItem(AGridItem);
end;

function TcxGridInplaceEditForm.GetLayoutLookAndFeel: TdxCustomLayoutLookAndFeel;
begin
  Result := nil;
end;

function TcxGridInplaceEditForm.GetVisible: Boolean;
begin
  Result := EditingRecordIndex <> cxGridInplaceEditFormEditingRecordIndexNone;
end;

procedure TcxGridInplaceEditForm.Init;
begin
  Container.Init;
end;

procedure TcxGridInplaceEditForm.StoreChildren(Proc: TGetChildProc);
begin
  Container.StoreChildren(Proc);
end;

procedure TcxGridInplaceEditForm.ValidateFocusedItem;
begin
  GridView.Controller.FocusFirstAvailableItem;
end;

function TcxGridInplaceEditForm.CloseOnRecordInserting: Boolean;
begin
  Result := Close;
end;

procedure TcxGridInplaceEditForm.CloseOnFocusedRecordChanging(var AFocusingRecord: TcxCustomGridRecord);
var
  ARecordIndex: Integer;
begin
  ARecordIndex := AFocusingRecord.RecordIndex;
  if Close then
    AFocusingRecord := GridView.ViewData.GetRecordByRecordIndex(ARecordIndex);
end;

function TcxGridInplaceEditForm.CloseQuery(ARaiseAbortOnCancel: Boolean = True): TModalResult;
var
  ADlgResult: Integer;
begin
  ADlgResult := MessageDlg(cxGetResourceString(@scxGridInplaceEditFormSaveChangesQuery),
    mtWarning, mbYesNoCancel, 0);
  if (ADlgResult = mrCancel) and ARaiseAbortOnCancel then
    Abort;
  Result := ADlgResult;
end;

procedure TcxGridInplaceEditForm.InvalidateUpdateButton;
var
  ARecordViewInfo: TcxCustomGridRecordViewInfo;
begin
  ARecordViewInfo := Controller.FocusedRecord.ViewInfo;
  if ARecordViewInfo is TcxGridDataRowViewInfo then
    TcxGridDataRowViewInfo(ARecordViewInfo).InplaceEditFormAreaViewInfo.InvalidateUpdateButtonInfo;
end;

procedure TcxGridInplaceEditForm.ResetEditingRecordIndex;
begin
  EditingRecordIndex := cxGridInplaceEditFormEditingRecordIndexNone;
end;

function TcxGridInplaceEditForm.NeedStretchRecordHeight: Boolean;
begin
  Result := (DefaultStretch in [fsVertical, fsClient]);
end;

function TcxGridInplaceEditForm.NeedStretchRecordWidth: Boolean;
begin
  Result := (DefaultStretch in [fsHorizontal, fsClient]);
end;

function TcxGridInplaceEditForm.GetContainerClass: TcxGridInplaceEditFormContainerClass;
begin
  Result := TcxGridInplaceEditFormContainer;
end;

function TcxGridInplaceEditForm.GetControllerClass: TcxGridInplaceEditFormControllerClass;
begin
  Result := TcxGridInplaceEditFormController;
end;

procedure TcxGridInplaceEditForm.CreateContainer;
begin
  FContainer := GetContainerClass.Create(Self);
  FContainer.Initialize;
  FContainer.LayoutLookAndFeel := LayoutLookAndFeel;
  FContainer.OnChanged := ContainerChangedHandler;
end;

procedure TcxGridInplaceEditForm.CreateController;
begin
  if Assigned(GridView) then
    FController := GetControllerClass.Create(GridView);
end;

procedure TcxGridInplaceEditForm.DestroyContainer;
begin
  FreeAndNil(FContainer);
end;

procedure TcxGridInplaceEditForm.DestroyController;
begin
  FreeAndNil(FController);
end;

procedure TcxGridInplaceEditForm.ContainerChangedHandler(Sender: TObject);
begin
  Changed;
end;

function TcxGridInplaceEditForm.GetDefaultColumnCount: Integer;
begin
  Result := FDefaultColumnCount;
end;

function TcxGridInplaceEditForm.GetItemHotTrack: Boolean;
begin
  Result := FItemHotTrack;
end;

function TcxGridInplaceEditForm.GetMasterRowDblClickAction: TcxGridMasterRowDblClickAction;
begin
  Result := FMasterRowDblClickAction;
end;

function TcxGridInplaceEditForm.GetEditingRecordIndex: Integer;
begin
  Result := FEditingRecordIndex;
end;

procedure TcxGridInplaceEditForm.SetDefaultColumnCount(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if (AValue <> DefaultColumnCount) then
  begin
    if Visible and not Close then
      Exit;
    FDefaultColumnCount := AValue;
    Changed;
  end;
end;

procedure TcxGridInplaceEditForm.SetItemHotTrack(AValue: Boolean);
begin
  if FItemHotTrack <> AValue then
  begin
    FItemHotTrack := AValue;
    Changed;
  end;
end;

procedure TcxGridInplaceEditForm.SetMasterRowDblClickAction(AValue: TcxGridMasterRowDblClickAction);
begin
  if FMasterRowDblClickAction <> AValue then
  begin
    FMasterRowDblClickAction := AValue;
    Changed;
  end;
end;

procedure TcxGridInplaceEditForm.SetEditingRecordIndex(AValue: Integer);
begin
  if (AValue <> FEditingRecordIndex) then
  begin
    if GridView.IsDesigning or not IsInplaceEditFormMode then
      AValue := cxGridInplaceEditFormEditingRecordIndexNone;
    if UseDefaultLayout then
    begin
      if AValue = cxGridInplaceEditFormEditingRecordIndexNone then
        Container.DestroyDefaultLayout
      else
        if not Container.IsDefaultLayoutCreated then
          Container.CreateDefaultLayout;
    end;
    FEditingRecordIndex := AValue;
    Changed(True);
    ValidateFocusedItem;
    AdjustEditingItemOnVisibilityChange;
  end;
end;

procedure TcxGridInplaceEditForm.SetDefaultStretch(AValue: TcxGridInplaceEditFormStretch);
begin
  if FDefaultStretch <> AValue then
  begin
    if Visible and not Close then
      Exit;
    FDefaultStretch := AValue;
    Container.UpdateRootOptions;
    Changed;
  end;
end;

procedure TcxGridInplaceEditForm.SetUseDefaultLayout(AValue: Boolean);
var
  ADlgResult: Integer;
begin
  if AValue <> FUseDefaultLayout then
  begin
    if AValue and not IsAssigningOptions and GridView.IsDesigning and
      not GridView.IsLoading then
    begin
      ADlgResult := MessageDlg(scxGridInplaceEditFormResetCustomSettingLayoutQuery,
        mtWarning, [mbYes, mbNo], 0);
      AValue := not (ADlgResult in [mrNo, mrCancel]);
    end;
    if AValue then
    begin
      Container.DestroyDefaultLayout;
      if Visible then
        Container.CreateDefaultLayout;
    end
    else
      if not Container.IsDefaultLayoutCreated then
        Container.CreateDefaultLayout;
    FUseDefaultLayout := AValue;
    Changed;
  end;
end;

initialization
  RegisterClasses([TcxGridInplaceEditFormBaseLayoutItem, TcxGridInplaceEditFormLayoutItem, TcxGridInplaceEditFormGroup]);

end.
