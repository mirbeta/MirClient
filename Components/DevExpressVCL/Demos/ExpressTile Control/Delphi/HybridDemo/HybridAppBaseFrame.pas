unit HybridAppBaseFrame;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB, Types,
  dxCustomTileControl, dxTileBar, HybridAppDM, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, cxTextEdit, cxMaskEdit, cxButtonEdit, cxClasses,
  dxLayoutControl, dxGDIPlusClasses, cxImage, cxGridTableView, cxGroupBox, cxCustomData, cxGridCustomTableView,
  dxCore;

const
  IDFirst = 0;
  IDTasks = IDFirst + 1;
  IDTaskPrint = IDFirst + 2;
  IDEmployees = IDFirst + 3;
  IDEmployeeEdit = IDFirst + 4;
  IDProducts = IDFirst + 5;
  IDProductEdit = IDFirst + 6;
  IDCustomers = IDFirst + 7;
  IDCustomerEdit = IDFirst + 8;
  IDSales = IDFirst + 9;
  IDSaleView = IDFirst + 10;
  IDSalesPrint = IDFirst + 11;
  IDLast = IDSalesPrint;

  UM_AFTERACTIVATE = WM_USER + 1;
  UM_BEFOREACTIVATE = WM_USER + 2;

type

  { TfrmBase }

  TfrmBase = class(TFrame, IdxLocalizerListener)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup2: TdxLayoutGroup;
    cxGroupBox1: TcxGroupBox;
    dxLayoutControl2Group_Root: TdxLayoutGroup;
    dxLayoutControl2: TdxLayoutControl;
    dxLayoutGroup4: TdxLayoutGroup;
    lgBackButton: TdxLayoutGroup;
    liiBackButton: TdxLayoutImageItem;
    procedure dxLayoutControl1Click(Sender: TObject);
  private
    FActivatingCount: Integer;

    procedure AfterActivate(var Message: TWMSize); message UM_AFTERACTIVATE;
    procedure BeforeActivate(var Message: TWMSize); message UM_BEFOREACTIVATE;
  protected
    procedure AssignGridFilterBoxFont(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList); virtual;
    function CanCloseQuery: Boolean; virtual;
    procedure DoAfterActivate; virtual;
    procedure DoAfterDeactivate; virtual;
    procedure DoBeforeActivate; virtual;
    procedure DoOnBackButtonClick; virtual;
    function GetDataSet: TDataSet; virtual;
    function GetParentFrameTileItem: TdxTileControlItem; virtual;
    function IsDataChanged: Boolean; virtual;
    procedure ReturnToParentFrame;
    procedure SaveData; virtual;
    procedure ShowEditPage(AItem: TdxTileBarItem); virtual;

    procedure RefreshInfo; virtual;
    procedure Translate; virtual;

    procedure TranslationChanged;

    property ActivatingCount: Integer read FActivatingCount;
    property DataSet: TDataSet read GetDataSet;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function CanDeactivate: Boolean; virtual;

    property ParentFrameTileItem: TdxTileControlItem read GetParentFrameTileItem;
  end;

  TfrmBaseClass = class of TfrmBase;

function GetDetailControlClass(ATag: Integer): TfrmBaseClass;
procedure RegisterFrame(AID: Integer; AFrameClass: TfrmBaseClass);

implementation

{$R *.dfm}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  MainUnit, cxGeometry, dxFilterPopupWindow, LocalizationStrs, dxFilterValueContainer;

type
  TdxCustomFilterValueContainerAcccess = class(TdxCustomFilterValueContainer);
  TdxTileBarItemAccess = class(TdxTileBarItem);
  TdxTileControlDetailSiteAccess = class(TdxTileControlDetailSite);
  TFilterPopupAccess = class(TdxFilterPopupWindow);

var
  AHybridAppFrameClasses: array [IDFirst..IDLast] of TfrmBaseClass;

{ TfrmBase }

procedure TfrmBase.AfterActivate(var Message: TWMSize);
begin
  DoAfterActivate;
end;

procedure TfrmBase.BeforeActivate(var Message: TWMSize);
begin
  DoBeforeActivate;
end;

procedure TfrmBase.AssignGridFilterBoxFont(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
var
  AStyle: TcxContainerStyle;
  AFilterPopup: TFilterPopupAccess;
  AViewParams: TcxViewParams;
begin
  Sender.Styles.GetContentParams(nil, AViewParams);
  AFilterPopup := TFilterPopupAccess(Sender.GridView.Controller.FilterPopup);
  AStyle := TdxCustomFilterValueContainerAcccess(AFilterPopup.FilterValueContainer).Style;
  AStyle.Font := AViewParams.Font;
  AStyle.Font.Color := AViewParams.TextColor;
end;

function TfrmBase.CanCloseQuery: Boolean;
begin
  Result := not IsDataChanged;
  if not Result then
  begin
    Result := MessageDlg(cxGetResourceString(@sReloadConfirmation), mtConfirmation, [mbOk, mbCancel], 0) = IDOK;
    if Result then
      DataSet.Cancel;
  end;
end;

procedure TfrmBase.AfterConstruction;
begin
  inherited AfterConstruction;
  dxResourceStringsRepository.AddListener(Self);
end;

procedure TfrmBase.BeforeDestruction;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  inherited BeforeDestruction;
end;

function TfrmBase.CanDeactivate: Boolean;
begin
  Result := not IsDataChanged or CanCloseQuery;
end;

procedure TfrmBase.DoAfterActivate;
begin
  Inc(FActivatingCount);

  Screen.Cursor := crHourGlass;
  try
    RefreshInfo;
    Translate;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmBase.DoAfterDeactivate;
begin
//
end;

procedure TfrmBase.DoBeforeActivate;
begin
//
end;

procedure TfrmBase.DoOnBackButtonClick;
begin
//
end;

procedure TfrmBase.dxLayoutControl1Click(Sender: TObject);
begin
  if (Sender = dxLayoutControl1) and lgBackButton.Visible and
      cxRectPtIn(liiBackButton.ViewInfo.Bounds, dxLayoutControl1.ScreenToClient(GetMouseCursorPos)) then
    DoOnBackButtonClick;
end;

function TfrmBase.GetDataSet: TDataSet;
begin
  Result := nil;
end;

function TfrmBase.GetParentFrameTileItem: TdxTileControlItem;
begin
  Result := nil;
end;

function TfrmBase.IsDataChanged: Boolean;
begin
  Result := (DataSet <> nil) and (DataSet.State = dsEdit);
end;

procedure TfrmBase.ReturnToParentFrame;
begin
  TdxTileControlDetailSiteAccess(MainForm.tbMain.ActiveDetail).DoDeactivate;
end;

procedure TfrmBase.SaveData;
begin
  if IsDataChanged then
    DataSet.Post;
end;

procedure TfrmBase.ShowEditPage(AItem: TdxTileBarItem);
var
  ATileBarItem: TdxTileBarItemAccess;
begin
  ATileBarItem := TdxTileBarItemAccess(AItem);
  if ATileBarItem.IsEnabled then       // the MainForm.tbMain remains same focused item
    ATileBarItem.DoClick;
end;

procedure TfrmBase.Translate;
begin
// do nothing
end;

procedure TfrmBase.RefreshInfo;
begin
// do nothing
end;

procedure TfrmBase.TranslationChanged;
begin
  Translate;
end;

function GetDetailControlClass(ATag: Integer): TfrmBaseClass;
begin
  Result := AHybridAppFrameClasses[ATag];
end;

procedure RegisterFrame(AID: Integer; AFrameClass: TfrmBaseClass);
begin
  AHybridAppFrameClasses[AID] := AFrameClass;
end;

end.
