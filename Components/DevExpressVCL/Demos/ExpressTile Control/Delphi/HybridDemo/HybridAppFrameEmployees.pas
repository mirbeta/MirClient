unit HybridAppFrameEmployees;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, cxTextEdit, cxMaskEdit, cxButtonEdit, dxLayoutControl, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, DB, cxDBData, ImgList, dxCustomTileControl, cxGridLevel,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxTileControl, HybridAppDM,
  cxImage, cxMemo, cxGridViewLayoutContainer, cxGridLayoutView, cxGridDBLayoutView, cxGridCustomLayoutView,
  dxLayoutControlAdapters, Menus, StdCtrls, cxButtons, dxCore, dxGDIPlusClasses, cxGroupBox, cxImageList;

type
  TfrmEmployees = class(TfrmBase)
    ilEmployees: TcxImageList;
    tcStatus: TdxTileControl;
    dxLayoutItem2: TdxLayoutItem;
    dxTileControl1Group1: TdxTileControlGroup;
    tiAll: TdxTileControlItem;
    tiSalaried: TdxTileControlItem;
    tiCommision: TdxTileControlItem;
    tiContract: TdxTileControlItem;
    tiTerminated: TdxTileControlItem;
    tiOnLeave: TdxTileControlItem;
    gvEmployeesLayout: TcxGridDBLayoutView;
    grEmployeesLevel1: TcxGridLevel;
    grEmployees: TcxGrid;
    dxLayoutItem3: TdxLayoutItem;
    lrFullName: TcxGridDBLayoutViewItem;
    lrPicture: TcxGridDBLayoutViewItem;
    lrAddress: TcxGridDBLayoutViewItem;
    lrPhone: TcxGridDBLayoutViewItem;
    lrEmail: TcxGridDBLayoutViewItem;
    dxLayoutItem1: TdxLayoutItem;
    btnSortAsc: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    btnSortDesc: TcxButton;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutItem8: TdxLayoutItem;
    btnEdit: TcxButton;
    procedure tcStatusItemFocusChange(Sender: TdxCustomTileControl; AFocusedItem, ANewFocusedItem: TdxTileControlItem);
    procedure btnSortAscClick(Sender: TObject);
    procedure btnSortDescClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure gvEmployeesLayoutCustomDrawRecordCaption(Sender: TcxGridLayoutView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridLayoutViewRecordCaptionViewInfo; var ADone: Boolean);
    procedure gvEmployeesLayoutCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure TranslateFilters;
  protected
    procedure Translate; override;
    procedure RefreshInfo; override;
  end;

implementation

{$R *.dfm}

uses
  MainUnit, LocalizationStrs;

{ TfrmEmployees }

procedure TfrmEmployees.btnEditClick(Sender: TObject);
var
  AEmployee_ID: Integer;
begin
  AEmployee_ID := DM.clEmployees.FieldByName('ID').AsInteger;
  DM.ReloadEmployeeTasks(AEmployee_ID);
  DM.ApplyEvalutionFilter(AEmployee_ID);
  ShowEditPage(MainForm.tbiEmployeeEdit);
end;

procedure TfrmEmployees.tcStatusItemFocusChange(Sender: TdxCustomTileControl; AFocusedItem,
  ANewFocusedItem: TdxTileControlItem);
begin
  if ANewFocusedItem <> nil then
    DM.ApplyEmployeesStatusFilter(ANewFocusedItem.Tag);
end;

procedure TfrmEmployees.btnSortAscClick(Sender: TObject);
begin
  lrFullName.SortOrder := soAscending;
end;

procedure TfrmEmployees.btnSortDescClick(Sender: TObject);
begin
  lrFullName.SortOrder := soDescending;
end;

procedure TfrmEmployees.Translate;
begin
  inherited Translate;
  TranslateFilters;

  lrAddress.Caption := cxGetResourceString(@sAddressLabel);
  lrPhone.Caption := cxGetResourceString(@sPhoneLabel);
  lrEmail.Caption := cxGetResourceString(@sEmailLabel);

  btnSortAsc.Caption := cxGetResourceString(@sAscendingButton);
  btnSortDesc.Caption := cxGetResourceString(@sDescendingButton);
  btnEdit.Caption := cxGetResourceString(@sEditButton);
end;

procedure TfrmEmployees.gvEmployeesLayoutCellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  btnEdit.Click;
end;

procedure TfrmEmployees.TranslateFilters;
var
  I: Integer;
  AText: string;
begin
  tcStatus.Title.Text := cxGetResourceString(@sEmployeeFilterCaption);
  for I := 0 to tcStatus.Items.Count - 1 do
  begin
    AText := DM.GetEmployeeStatusName(TdxEmployeeStatus(tcStatus.Items[I].Tag));
    if AText = '' then
    begin
      if tcStatus.Items[I].Tag = cAllCountTag then
        AText := cxGetResourceString(@sFilterAllCaption)
      else
        AText := tcStatus.Items[I].Text3.Value;
    end;
    tcStatus.Items[I].Text3.Value := AText;
  end;
end;

procedure TfrmEmployees.gvEmployeesLayoutCustomDrawRecordCaption(Sender: TcxGridLayoutView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridLayoutViewRecordCaptionViewInfo; var ADone: Boolean);
begin
  AViewInfo.Text := AViewInfo.GridRecord.Values[TcxGridDBLayoutView(AViewInfo.GridView).GetItemByFieldName('FullName').Index];
end;

procedure TfrmEmployees.RefreshInfo;
begin
  inherited RefreshInfo;

  DM.RecalculateEmployeesStatusCount;

  tiAll.Tag := cAllCountTag;
  tiAll.Text2.Value := IntToStr(DM.EmployeeStatusCount[cAllCountTag]);

  tiSalaried.Tag := Integer(esSalaried);
  tiSalaried.Text2.Value := IntToStr(DM.EmployeeStatusCount[tiSalaried.Tag]);

  tiCommision.Tag := Integer(esCommission);
  tiCommision.Text2.Value := IntToStr(DM.EmployeeStatusCount[tiCommision.Tag]);

  tiContract.Tag := Integer(esContract);
  tiContract.Text2.Value := IntToStr(DM.EmployeeStatusCount[Integer(tiContract.Tag)]);

  tiTerminated.Tag := Integer(esTerminated);
  tiTerminated.Text2.Value := IntToStr(DM.EmployeeStatusCount[tiTerminated.Tag]);

  tiOnLeave.Tag := Integer(esOnLeave);
  tiOnLeave.Text2.Value := IntToStr(DM.EmployeeStatusCount[tiOnLeave.Tag]);

  if tcStatus.Controller.FocusedItem = nil then
    tcStatus.Controller.FocusedItem := tiAll;
end;

initialization
  RegisterFrame(IDEmployees, TfrmEmployees);

end.
