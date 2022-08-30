{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPScxGridLayoutViewLnk;

{$I cxVer.Inc}

interface

uses
  Types, SysUtils, cxDBData, dxLayoutContainer, dxLayoutLookAndFeels, cxGridCustomView, cxGridCustomLayoutView,
  cxGridLayoutView, cxStyles, cxGridCustomTableView, cxGraphics, dxPScxCommon,
  cxGridDBDataDefinitions, cxGridDBLayoutView, dxPSCore, dxPScxGridLnk, dxPSdxLCLnk;

type
  TdxGridLayoutViewAdapter = class;
  TdxGridLayoutViewBuilder = class;

  TdxReportLayoutRecord = class(TdxReportCustomLayoutRecord)
  private
    FGroupLookAndFeel: TdxPSReportLayoutGroupStandardLookAndFeel;
    function GetContainer: TdxLayoutContainer;
    function GetLayoutRecord: TcxGridLayoutViewRecord;
  protected
    property Container: TdxLayoutContainer read GetContainer;
    property LayoutRecord: TcxGridLayoutViewRecord read GetLayoutRecord;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
  end;

  TdxReportGridLayoutItem = class(TdxReportLayoutLabeledItem)
  private
    FDataCell: TAbstractdxReportCellData;
    function GetGridRecord: TcxCustomGridRecord;
    function GetGridTableItem: TcxCustomGridTableItem;
    function GetViewInfo: TcxGridLayoutItemViewInfo;
  protected
    procedure Initialize(AReportLink: TBasedxReportLink); override;

    function CreateDataCell: TAbstractdxReportCellData;
  public
    destructor Destroy; override;

    property DataCell: TAbstractdxReportCellData read FDataCell;
    property GridRecord: TcxCustomGridRecord read GetGridRecord;
    property GridTableItem: TcxCustomGridTableItem read GetGridTableItem;
    property ViewInfo: TcxGridLayoutItemViewInfo read GetViewInfo;
  end;

  TdxGridLayoutViewFormatter = class(TdxGridCustomLayoutViewFormatter, IcxGridLayoutViewStylesHelper)
  private
    FCalculatingRecordViewInfo: TcxGridLayoutViewRecordViewInfo;
    function GetItemBounds(ARecordViewInfo: TcxGridLayoutViewRecordViewInfo; AViewInfo: TdxCustomLayoutItemViewInfo): TRect;
    function GetLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
    procedure GetViewInfoHandler(AItem: TdxCustomLayoutItem; var AViewInfo: TdxCustomLayoutItemViewInfo);
  protected
    function GetFirstRecordOffset: Integer; override;
    function GetInterRecordsSpaceHorz: Integer; override;
    function GetInterRecordsSpaceVert: Integer; override;

    procedure CreateCaptionCell(ARecord: TdxReportLayoutRecord);
    procedure DoCreateCaptionCell(ARecord: TdxReportLayoutRecord);

    procedure InitializeRecordCells(ARecord: TdxReportLayoutRecord);
    function GetReportItemClass(ALayoutItem: TdxCustomLayoutItem): TdxCustomReportLayoutItemClass;

    function GetGroupViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
    function GetItemCaptionViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
    function GetRecordCaptionViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
    function GetRecordViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;

    // IcxGridLayoutViewStylesHelper
    procedure GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
    procedure GetRecordCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);

    property LayoutLookAndFeel: TcxGridLayoutLookAndFeel read GetLayoutLookAndFeel;
  public
    function Adapter: TdxGridLayoutViewAdapter; reintroduce; overload;
    function Builder: TdxGridLayoutViewBuilder; reintroduce; overload;
    procedure DoInitializeRecord(ARecord: TdxReportCustomLayoutRecord; AGridRecord: TcxGridCustomLayoutRecord); override;

    function GetItemViewParams(ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams; override;

    function GetRecordClass(AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecordClass; override;
  end;

  { TdxGridLayoutViewAdapter }

  TdxGridLayoutViewAdapter = class(TdxGridCustomLayoutViewAdapter)
  protected
    function Styles: TcxGridLayoutViewStyles; reintroduce; overload;
  end;

  { TdxGridLayoutViewBuilder }

  TdxGridLayoutViewBuilder = class(TdxGridCustomLayoutViewBuilder)
  private
    function GetRecord(Index: Integer): TdxReportLayoutRecord;
  protected
    function GridView: TcxGridLayoutView; reintroduce; overload;
    class function GridViewClass: TcxCustomGridViewClass; override;
    procedure DoBuildViewBody; override;
    procedure DoResizeRecords; override;

    property Records[Index: Integer]: TdxReportLayoutRecord read GetRecord;
  public
    constructor Create(AReportLink: TdxGridReportLink; AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    class function AdapterClass: TdxGridViewAdapterClass; override;
    function Formatter: TdxGridLayoutViewFormatter; reintroduce; overload;
    class function FormatterClass: TdxGridViewFormatterClass; override;
  end;

  TdxGridDBLayoutViewAdapter = class(TdxGridLayoutViewAdapter)
  protected
    function DataController: TcxGridDBDataController; reintroduce; overload;
    function DBDataModeController: TcxDBDataModeController; override;
  end;

  TdxGridDBLayoutViewBuilder = class(TdxGridLayoutViewBuilder)
  protected
    class function GridViewClass: TcxCustomGridViewClass; override;
  public
    class function AdapterClass: TdxGridViewAdapterClass; override;
  end;

implementation

uses
  Classes, Graphics, Contnrs, cxGeometry, cxEdit, dxCore, cxGridViewLayoutContainer;

const
  FirstRecordOffset = 2;

type
  TdxCustomReportLayoutItemAccess = class(TdxCustomReportLayoutItem);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TdxPSReportGroupLookAndFeelAccess = class(TdxPSReportGroupLookAndFeel);
  TcxGridLayoutContainerAccess = class(TcxGridLayoutContainer);
  TcxGridLayoutViewAccess = class(TcxGridLayoutView);
  TBasedxReportLinkAccess = class(TBasedxReportLink);
  TdxLayoutLookAndFeelGroupOptionsAccess = class(TdxLayoutLookAndFeelGroupOptions);
  TcxStylesAccess = class(TcxStyles);

{ TdxReportLayoutRecord }

constructor TdxReportLayoutRecord.Create(AParent: TdxReportCell);
begin
  inherited;
  FGroupLookAndFeel := TdxPSReportLayoutGroupStandardLookAndFeel.Create(ReportCells);
end;

destructor TdxReportLayoutRecord.Destroy;
begin
  FreeAndNil(FGroupLookAndFeel);
  inherited;
end;

function TdxReportLayoutRecord.GetContainer: TdxLayoutContainer;
begin
  Result := LayoutRecord.GridView.Container;
end;

function TdxReportLayoutRecord.GetLayoutRecord: TcxGridLayoutViewRecord;
begin
  Result := TcxGridLayoutViewRecord(Data);
end;

{ TdxReportGridLayoutItem }

destructor TdxReportGridLayoutItem.Destroy;
begin
  FreeAndNil(FDataCell);
  inherited;
end;

procedure TdxReportGridLayoutItem.Initialize(AReportLink: TBasedxReportLink);
begin
  inherited;
  FDataCell := CreateDataCell;
end;

function TdxReportGridLayoutItem.CreateDataCell: TAbstractdxReportCellData;

  function GetItemProperties: TcxCustomEditProperties;
  begin
    Result := GridTableItem.GetProperties(GridRecord);
    if Result = nil then
      Result := GridTableItem.GetRepositoryItem.Properties;
  end;

begin
  Result := dxPSDataMaps.ItemClass(GetItemProperties, False).Create(Self);
  Result.BoundsRect := cxRectOffset(ViewInfo.ControlViewInfo.Bounds, ViewInfo.Bounds.TopLeft, False);
end;

function TdxReportGridLayoutItem.GetGridRecord: TcxCustomGridRecord;
begin
  Result := ViewInfo.GridItemViewInfo.GridRecord;
end;

function TdxReportGridLayoutItem.GetGridTableItem: TcxCustomGridTableItem;
begin
  Result := ViewInfo.GridItemViewInfo.Item;
end;

function TdxReportGridLayoutItem.GetViewInfo: TcxGridLayoutItemViewInfo;
begin
  Result := inherited ViewInfo as TcxGridLayoutItemViewInfo;
end;

{ TdxGridLayoutViewFormatter }

function TdxGridLayoutViewFormatter.Builder: TdxGridLayoutViewBuilder;
begin
  Result := inherited Builder as TdxGridLayoutViewBuilder;
end;

function TdxGridLayoutViewFormatter.Adapter: TdxGridLayoutViewAdapter;
begin
  Result := TdxGridLayoutViewAdapter(inherited Adapter);
end;

procedure TdxGridLayoutViewFormatter.DoInitializeRecord(ARecord: TdxReportCustomLayoutRecord; AGridRecord: TcxGridCustomLayoutRecord);
begin
  inherited;
  InitializeRecordCells(ARecord as TdxReportLayoutRecord);
end;

function TdxGridLayoutViewFormatter.GetItemViewParams(ATableItem: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; AnIsPreview: Boolean = False; AIsDataCell: Boolean = False): TdxReportItemViewParams;
begin
  Result := inherited GetItemViewParams(ATableItem, ARecord, AnIsPreview, AIsDataCell);
  Result.Transparent := not AIsDataCell;
end;

function TdxGridLayoutViewFormatter.GetRecordClass(AGridRecord: TcxGridCustomLayoutRecord): TdxReportCustomLayoutRecordClass;
begin
  Result := TdxReportLayoutRecord;
end;

function TdxGridLayoutViewFormatter.GetFirstRecordOffset: Integer;
begin
  Result := FirstRecordOffset;
end;

function TdxGridLayoutViewFormatter.GetInterRecordsSpaceHorz: Integer;
begin
  Result := ReportLink.OptionsCards.InterCardsSpaceHorz;
end;

function TdxGridLayoutViewFormatter.GetInterRecordsSpaceVert: Integer;
begin
  Result := ReportLink.OptionsCards.InterCardsSpaceVert;
end;

procedure TdxGridLayoutViewFormatter.CreateCaptionCell(ARecord: TdxReportLayoutRecord);
begin
  if FCalculatingRecordViewInfo.CaptionViewInfo <> nil then
    DoCreateCaptionCell(ARecord);
end;

procedure TdxGridLayoutViewFormatter.DoCreateCaptionCell(ARecord: TdxReportLayoutRecord);
var
  R: TRect;
  ACaption: TdxReportCellString;
begin
  ACaption := TdxReportCellString.Create(ARecord);
  SetViewParams(ACaption, GetRecordCaptionViewParams(ARecord.LayoutRecord));
  with ACaption do
  begin
    R := cxRectOffset(FCalculatingRecordViewInfo.CaptionViewInfo.Bounds, FCalculatingRecordViewInfo.Bounds.TopLeft, False);
    Text := FCalculatingRecordViewInfo.CaptionViewInfo.Text;
    BoundsRect := R;
  end;
end;

procedure TdxGridLayoutViewFormatter.InitializeRecordCells(ARecord: TdxReportLayoutRecord);

  procedure InitializeCell(AViewInfo: TdxCustomLayoutItemViewInfo);
  var
    I: Integer;
    AReportItem: TdxCustomReportLayoutItem;
    AItem: TdxCustomLayoutItem;
  begin
    if TdxCustomLayoutItemViewInfoAccess(AViewInfo).ActuallyVisible then
    begin
      AItem := TdxCustomLayoutItemViewInfoAccess(AViewInfo).Item;
      AReportItem := GetReportItemClass(AItem).Create(ARecord);
      AReportItem.Data := TdxNativeInt(AItem);
      AReportItem.BoundsRect := GetItemBounds(FCalculatingRecordViewInfo, AViewInfo);

      AReportItem.OnGetLayoutItemViewInfo := GetViewInfoHandler;
      try
        if AViewInfo is TdxLayoutGroupViewInfo then
          SetViewParams(AReportItem, GetGroupViewParams(ARecord.LayoutRecord))
        else
          SetViewParams(AReportItem, GetItemCaptionViewParams(ARecord.LayoutRecord));
        TdxCustomReportLayoutItemAccess(AReportItem).Initialize(Builder.ReportLink);
        if AReportItem is TdxReportGridLayoutItem then
          with AReportItem as TdxReportGridLayoutItem do
            DoInitializeItem(DataCell, GridTableItem, GridRecord, False);
        if AViewInfo is TdxLayoutGroupViewInfo then
        begin
          AReportItem.LookAndFeel := ARecord.FGroupLookAndFeel;
          if TdxCustomLayoutItemViewInfoAccess(AViewInfo).HasBorder then
            AReportItem.CellSides := TdxPSReportGroupLookAndFeelAccess(AReportItem.LookAndFeel).GetBorderSides(AReportItem)
          else
            AReportItem.CellSides := [];
          for I := 0 to (AViewInfo as TdxLayoutGroupViewInfo).ItemViewInfoCount - 1 do
            InitializeCell((AViewInfo as TdxLayoutGroupViewInfo).ItemViewInfos[I]);
        end;
      finally
        AReportItem.OnGetLayoutItemViewInfo := nil;
      end;
    end;
  end;

  procedure CalculateRecordViewInfo;
  begin
    with FCalculatingRecordViewInfo do
      Calculate(10000, 10000);
  end;

begin
  FCalculatingRecordViewInfo := TcxGridLayoutViewRecordViewInfo.Create(ARecord.LayoutRecord.GridView.ViewInfo.RecordsViewInfo, ARecord.LayoutRecord);
  try
    CalculateRecordViewInfo;
    ARecord.Width := FCalculatingRecordViewInfo.Width;
    ARecord.Height := FCalculatingRecordViewInfo.Height;
    CreateCaptionCell(ARecord);
    SetViewParams(ARecord, GetRecordViewParams(ARecord.LayoutRecord));
    if ARecord.LayoutRecord.Expanded then
      InitializeCell(FCalculatingRecordViewInfo.LayoutViewInfo.ItemsViewInfo);
  finally
    FreeAndNil(FCalculatingRecordViewInfo);
  end;
end;

function TdxGridLayoutViewFormatter.GetReportItemClass(ALayoutItem: TdxCustomLayoutItem): TdxCustomReportLayoutItemClass;
begin
  if ALayoutItem is TdxLayoutGroup then
    Result := TdxReportLayoutGroup
  else
    if ALayoutItem is TcxGridLayoutItem then
      Result := TdxReportGridLayoutItem
    else
      if ALayoutItem is TdxLayoutEmptySpaceItem then
        Result := TdxReportLayoutEmptySpaceItem
      else
        if ALayoutItem is TdxLayoutLabeledItem then
          Result := TdxReportLayoutLabeledItem
        else
          if ALayoutItem is TdxLayoutSeparatorItem then
            Result := TdxReportLayoutSeparatorItem
          else
            if ALayoutItem is TdxLayoutSplitterItem then
              Result := TdxReportLayoutSplitterItem
            else
              Result := TdxCustomReportLayoutItem;
end;

function TdxGridLayoutViewFormatter.GetGroupViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
var
  AParams: TcxViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
  begin
    TcxStylesAccess(ReportLink.Styles).GetDefaultViewParams(vspsGridGroup, nil, AParams);
    ReportLink.Styles.GetGroupParams(ARecord, 0, Result.NativeParams);
    Result.Transparent := Result.NativeParams.Color = AParams.Color;
  end
  else
  begin
    Adapter.Styles.GetGroupParams(ARecord, nil, Result.NativeParams);
    if Result.NativeParams.Color = clDefault then
      Result.NativeParams.Color := TdxLayoutLookAndFeelGroupOptionsAccess(LayoutLookAndFeel.GroupOptions).GetDefaultColor;
    Result.Transparent := False;
  end;
  Result.FontStyle := [];
end;

function TdxGridLayoutViewFormatter.GetItemCaptionViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetCardRowCaptionParams(ARecord, nil, Result.NativeParams)
  else
    Adapter.Styles.GetItemParams(ARecord, nil, Result.NativeParams);
  Result.FontStyle := [];
  Result.CellSides := [];
  Result.Transparent := True;
end;

function TdxGridLayoutViewFormatter.GetRecordCaptionViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetCardCaptionRowParams(ARecord, nil, Result.NativeParams)
  else
    Adapter.Styles.GetRecordCaptionParams(ARecord, Result.NativeParams);
  if ReportLink.OptionsCards.Borders then
    Result.CellSides := csAll
  else
    Result.CellSides := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);
  Result.FontStyle := [];
end;

function TdxGridLayoutViewFormatter.GetRecordViewParams(ARecord: TcxCustomGridRecord): TdxReportItemViewParams;
begin
  Result := GetGroupViewParams(ARecord);
  if ReportLink.OptionsCards.Borders then
    Result.CellSides := csAll
  else
    Result.CellSides := [];
  Result.FontStyle := [];
end;

procedure TdxGridLayoutViewFormatter.GetContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    AParams := GetItemViewParams(AItem, ARecord, True).NativeParams
  else
    Adapter.Styles.GetContentParams(ARecord, AItem, AParams);
end;

procedure TdxGridLayoutViewFormatter.GetGroupParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  AParams := GetGroupViewParams(ARecord).NativeParams;
end;

procedure TdxGridLayoutViewFormatter.GetItemParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  AParams := GetItemCaptionViewParams(ARecord).NativeParams;
end;

procedure TdxGridLayoutViewFormatter.GetRecordCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams);
begin
  AParams := GetRecordCaptionViewParams(ARecord).NativeParams;
end;

function TdxGridLayoutViewFormatter.GetItemBounds(ARecordViewInfo: TcxGridLayoutViewRecordViewInfo; AViewInfo: TdxCustomLayoutItemViewInfo): TRect;
begin
  Result := cxRectOffset(AViewInfo.Bounds, ARecordViewInfo.Bounds.TopLeft, False);
end;

function TdxGridLayoutViewFormatter.GetLayoutLookAndFeel: TcxGridLayoutLookAndFeel;
begin
  Result := TcxGridLayoutViewAccess(Builder.GridView).LayoutLookAndFeel;
end;

procedure TdxGridLayoutViewFormatter.GetViewInfoHandler(AItem: TdxCustomLayoutItem; var AViewInfo: TdxCustomLayoutItemViewInfo);
begin
  AViewInfo := FCalculatingRecordViewInfo.LayoutViewInfo.GetItemViewInfo(AItem);
end;

{ TdxGridLayoutViewAdapter }

function TdxGridLayoutViewAdapter.Styles: TcxGridLayoutViewStyles;
begin
  Result := inherited Styles as TcxGridLayoutViewStyles;
end;

{ TdxGridLayoutViewBuilder }

constructor TdxGridLayoutViewBuilder.Create(AReportLink: TdxGridReportLink;
  AMasterBuilder: TdxCustomGridViewBuilder; AGridView: TcxCustomGridView);
begin
  // todo:
  inherited;
end;

destructor TdxGridLayoutViewBuilder.Destroy;
begin
  // todo:
  inherited;
end;

class function TdxGridLayoutViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridLayoutViewAdapter;
end;

function TdxGridLayoutViewBuilder.Formatter: TdxGridLayoutViewFormatter;
begin
  Result := inherited Formatter as TdxGridLayoutViewFormatter;
end;

class function TdxGridLayoutViewBuilder.FormatterClass: TdxGridViewFormatterClass;
begin
  Result := TdxGridLayoutViewFormatter;
end;

function TdxGridLayoutViewBuilder.GridView: TcxGridLayoutView;
begin
  Result := inherited GridView as TcxGridLayoutView;
end;

class function TdxGridLayoutViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridLayoutView;
end;

procedure TdxGridLayoutViewBuilder.DoBuildViewBody;

  procedure UpdateGridViewSize;
  begin
    TcxGridLayoutViewAccess(GridView).IsDefaultViewInfoCalculated := False;
    GridView.Changed(vcSize);
  end;

var
  AList: TObjectList;
begin
  AList := TObjectList.Create;
  dxLayoutStoreItemStates(AList, GridView.Container);
  try
    GridView.StylesAdapter.Helper := Formatter;
    dxLayoutSetItemStates(GridView.Container, True, False, False, False);
    UpdateGridViewSize;
    inherited;
    GridView.StylesAdapter.Helper := nil;
  finally
    dxLayoutRestoreItemStates(AList, GridView.Container);
    AList.Free;
  end;
  UpdateGridViewSize;
end;

procedure TdxGridLayoutViewBuilder.DoResizeRecords;

  function GetRootWidth(ARecord: TdxReportCustomLayoutRecord): Integer;
  begin
    Result := ARecord.Width - 2 * GridView.OptionsView.RecordBorderWidth;
  end;

var
  I: Integer;
begin
  inherited;
  if Formatter.AutoWidth then
  begin
    for I := 0 to RecordCount - 1 do
    begin
      GridView.Container.Root.Width := GetRootWidth(Records[I]);
      try
        Records[I].ClearAll;
        Formatter.InitializeRecordCells(Records[I]);
      finally
        GridView.Container.Root.Width := 0;
      end;
    end;
  end;
end;

function TdxGridLayoutViewBuilder.GetRecord(Index: Integer): TdxReportLayoutRecord;
begin
  Result := inherited Records[Index] as TdxReportLayoutRecord;
end;

{ TdxGridDBLayoutViewAdapter }

function TdxGridDBLayoutViewAdapter.DataController: TcxGridDBDataController;
begin
  Result := inherited DataController as TcxGridDBDataController;
end;

function TdxGridDBLayoutViewAdapter.DBDataModeController: TcxDBDataModeController;
begin
  Result := DataController.DataModeController;
end;

{ TdxGridDBLayoutViewBuilder }

class function TdxGridDBLayoutViewBuilder.AdapterClass: TdxGridViewAdapterClass;
begin
  Result := TdxGridDBLayoutViewAdapter;
end;

class function TdxGridDBLayoutViewBuilder.GridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBLayoutView;
end;

procedure RegisterAssistants;
begin
  TdxGridLayoutViewBuilder.Register;
  TdxGridDBLayoutViewBuilder.Register;
end;

procedure UnegisterAssistants;
begin
  TdxGridDBLayoutViewBuilder.Unregister;
  TdxGridLayoutViewBuilder.Unregister;
end;

initialization
  RegisterAssistants;


finalization
  UnegisterAssistants;

end.
