unit GridModeDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxControls, cxGridCustomView, cxGrid, cxCustomData,
  ExtCtrls, ActnList, ImgList, Menus, ComCtrls, cxGridCustomPopupMenu,
  cxGridPopupMenu, cxStyles, cxGraphics, cxFilter, cxData, cxEdit, DB,
  cxDBData, cxClasses, Grids, DBGrids, cxLookAndFeelPainters, cxButtons, 
  cxDBLookupComboBox, DBTables, cxDataStorage, cxBlobEdit, cxSpinEdit,
  cxCheckBox, cxHyperLinkEdit, cxCurrencyEdit, cxCalendar, cxImageComboBox,
  cxCalc, cxLookAndFeels, BaseForm, cxGridCardView, cxContainer, cxProgressBar,
  cxNavigator, dxCore;

const
  DataPath = '..\..\Data\';
  CarsTableNames: array[0..2] of string = ('Customers','Orders','Cars');

type
  TGridModeDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Grid: TcxGrid;
    lvCars: TcxGridLevel;
    tvCars: TcxGridDBTableView;
    tvCarsTrademark: TcxGridDBColumn;
    tvCarsModel: TcxGridDBColumn;
    tvCarshp: TcxGridDBColumn;
    tvCarsliter: TcxGridDBColumn;
    tvCarscyl: TcxGridDBColumn;
    tvCarsTransmissSpeedCount: TcxGridDBColumn;
    tvCarsTransmissAutomatic: TcxGridDBColumn;
    tvCarsMPG_City: TcxGridDBColumn;
    tvCarsMPG_Highway: TcxGridDBColumn;
    tvCarsCategory: TcxGridDBColumn;
    tvCarsDescription: TcxGridDBColumn;
    tvCarsHyperlink: TcxGridDBColumn;
    tvCarsPrice: TcxGridDBColumn;
    lvOrders: TcxGridLevel;
    tvOrders: TcxGridDBTableView;
    tvOrdersCustomerID: TcxGridDBColumn;
    tvOrdersPurchaseDate: TcxGridDBColumn;
    tvOrdersPaymentType: TcxGridDBColumn;
    tvOrdersPaymentAmount: TcxGridDBColumn;
    pnlPopulate: TPanel;
    btnPopulate: TcxButton;
    N1: TMenuItem;
    miCustomizeViews: TMenuItem;
    miCalculateSummaries: TMenuItem;
    miEnableSorting: TMenuItem;
    miEnableFiltering: TMenuItem;
    pnlProgress: TPanel;
    ProgressBar: TcxProgressBar;
    miRecreateDB: TMenuItem;
    procedure miCalculateSummariesClick(Sender: TObject);
    procedure miEnableSortingClick(Sender: TObject);
    procedure miEnableFilteringClick(Sender: TObject);
    procedure miRecreateDBClick(Sender: TObject);
    procedure tvDataControllerFilterGetValueList(
      Sender: TcxFilterCriteria; AItemIndex: Integer;
      AValueList: TcxDataFilterValueList);
    procedure tvDataControllerSortingChanged(Sender: TObject);
    procedure tvOrdersDataControllerSummaryAfterSummary(
      ASender: TcxDataSummary);
    procedure btnPopulateClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  private
    FStartIDValue, FEndIDValue: Integer;
    procedure tblOrdersPopulate;
    function GetSQLCondition(AView: TcxGridDBTableView; AAddFilter: Boolean): string;
    procedure InitPopulateButton;
    procedure OpenTables(AOpen: Boolean);
    procedure CopyDBToLocalPlace(ARewrite: Boolean);
  end;

var
  GridModeDemoMainForm: TGridModeDemoMainForm;

implementation

{$R *.dfm}

uses
  GridModeDemoData,
  cxGridDBDataDefinitions, GridModeDemoTerminate, AboutDemoForm;

procedure TGridModeDemoMainForm.miCalculateSummariesClick(Sender: TObject);
var
  AEnableSummaries: Boolean;
  AAfterSummaryEvent: TcxAfterSummaryEvent;

  procedure EnableCarsSummaries;
  begin
    tvCars.BeginUpdate;
    try
      tvCars.DataController.Summary.OnAfterSummary := AAfterSummaryEvent;
      tvCars.OptionsView.Footer := AEnableSummaries;
    finally
      tvCars.EndUpdate;
    end;
    tvCars.DataController.ClearDetails;
  end;

  procedure EnableOrdersSummaries;
  begin
    tvOrders.BeginUpdate;
    try
      tvOrders.DataController.Summary.OnAfterSummary := AAfterSummaryEvent;
      tvOrders.OptionsView.Footer := AEnableSummaries;
    finally
      tvOrders.EndUpdate;
    end;
  end;

begin
  AEnableSummaries := GetMenuItemChecked(Sender);
  if AEnableSummaries then
    AAfterSummaryEvent := tvOrdersDataControllerSummaryAfterSummary
  else
    AAfterSummaryEvent := nil;
  EnableOrdersSummaries;
  EnableCarsSummaries;
  if AEnableSummaries then
  begin
    tvCars.DataController.Summary.BeginUpdate;
    tvCars.DataController.Summary.EndUpdate;
  end;
end;

procedure TGridModeDemoMainForm.miEnableSortingClick(Sender: TObject);
var
  AEnableSorting: Boolean;
  ASortingChangedEvent: TNotifyEvent;

  procedure EnableSorting(AView: TcxGridDBTableView);
  begin
    AView.DataController.ClearSorting(False);
    AView.DataController.OnSortingChanged := ASortingChangedEvent;
    AView.OptionsCustomize.ColumnSorting := AEnableSorting;
  end;

begin
  AEnableSorting := GetMenuItemChecked(Sender);
  if AEnableSorting then
    ASortingChangedEvent := tvDataControllerSortingChanged
  else
    ASortingChangedEvent := nil;
  EnableSorting(tvOrders);
  EnableSorting(tvCars);
  tvCars.DataController.ClearDetails;
end;

procedure TGridModeDemoMainForm.miEnableFilteringClick(Sender: TObject);
var
  AEnableFiltering: Boolean;
begin
  AEnableFiltering := GetMenuItemChecked(Sender);
  if not tvOrders.DataController.Filter.IsEmpty then
    tvOrders.DataController.Filter.Clear;
  if not tvCars.DataController.Filter.IsEmpty then
    tvCars.DataController.Filter.Clear;
  tvOrders.OptionsCustomize.ColumnFiltering := AEnableFiltering;
  tvCars.OptionsCustomize.ColumnFiltering := AEnableFiltering;
  tvCars.DataController.ClearDetails;
end;

procedure TGridModeDemoMainForm.miRecreateDBClick(Sender: TObject);
begin
  CopyDBToLocalPlace(True);
end;

procedure TGridModeDemoMainForm.tvDataControllerFilterGetValueList(
  Sender: TcxFilterCriteria; AItemIndex: Integer;
  AValueList: TcxDataFilterValueList);
var
  AColumn: TcxGridDBColumn;
  ADataSet: TDataSet;
  AProperties: TcxLookupComboBoxProperties;
  AValue: Variant;
  AIsLookupColumn: Boolean;
begin
  ADataSet := TcxGridDBTableView(Grid.FocusedView).DataController.DataSet;
  AColumn := TcxGridDBTableView(Grid.FocusedView).Columns[AItemIndex];
  AIsLookupColumn := AColumn.PropertiesClass = TcxLookupComboBoxProperties;
  with GridModeDemoDataDM do
    try
      Screen.Cursor := crHourGlass;
      qryHelper.SQL.Clear;
      qryHelper.SQL.Add(
        'Select DISTINCT ' + AColumn.DataBinding.FieldName + ' From ' +
        GetTableNameByDataSet(ADataSet));
      qryHelper.SQL.Add(
        GetSQLCondition(TcxGridDBTableView(Grid.FocusedView), False));
      qryHelper.Open;
      qryHelper.First;
      while not qryHelper.Eof do
      begin
        AValue := qryHelper.Fields[0].Value;
        if AIsLookupColumn then
        begin
          AProperties := TcxLookupComboBoxProperties(AColumn.GetProperties);
          ADataSet := AProperties.ListSource.DataSet;
          AValue := ADataSet.Lookup(
            AProperties.KeyFieldNames,AValue, AProperties.ListFieldNames);
        end;
        if VarIsNull(AValue) then Exit;
        AValueList.Add(fviValue, qryHelper.Fields[0].Value, AValue, False);
        qryHelper.Next;
      end;
      qryHelper.Close;
    finally
      Screen.Cursor := crDefault;
    end;
end;

procedure TGridModeDemoMainForm.tvDataControllerSortingChanged(Sender: TObject);

  procedure SortClone;
    var
      AColumn: TcxGridDBColumn;
      AFieldName: string;
      I: Integer;
  begin
    with TcxGridDBDataController(Sender).GridView do
    begin
      if not PatternGridView.IsPattern then Exit;
      try
        BeginUpdate;
        TcxGridDBTableView(PatternGridView).BeginUpdate;
        TcxGridDBTableView(PatternGridView).DataController.ClearSorting(False);
        for I := 0 to SortedItemCount - 1 do
        begin
          AFieldName := TcxGridDBColumn(SortedItems[I]).DataBinding.FieldName;
          AColumn :=
            TcxGridDBTableView(PatternGridView).GetColumnByFieldName(AFieldName);
          if AColumn.SortOrder <> SortedItems[I].SortOrder then
           AColumn.SortOrder := SortedItems[I].SortOrder;
        end;
      finally
        TcxGridDBTableView(PatternGridView).EndUpdate;
        EndUpdate;
      end;
    end;

  end;

  procedure ApplySortToQuery(AQuery: TQuery; ASortArray: array of string);
  var
    I: Integer;
    ASortString: string;
  begin
    ASortString := '';
    for I := 0 to High(ASortArray) do
      ASortString := ASortString + ASortArray[I];
    Delete(ASortString, Length(ASortString)-1, 2);
    try
      AQuery.DisableControls;
      AQuery.Close;
      if ASortString <> '' then ASortString := 'order by ' + ASortString;
      if AQuery.SQL.Count > 1 then AQuery.SQL[1] := ASortString
      else AQuery.SQL.Add(ASortString);
    finally
      AQuery.Open;
      AQuery.EnableControls;
    end;
  end;

  procedure SortPattern;
  var
    I: Integer;
    AOrder, AFieldName: string;
    ASortArray: array of string;
  begin
    with TcxGridDBDataController(Sender).GridView do
    try
      BeginUpdate;
      SetLength(ASortArray, SortedItemCount);
      for I := 0 to SortedItemCount - 1 do
      begin
        AFieldName := TcxGridDBColumn(SortedItems[I]).DataBinding.FieldName;
        if SortedItems[I].SortOrder = soAscending then
          AOrder := ' ASC, '
        else
          AOrder := ' DESC, ';
        ASortArray[SortedItems[I].SortIndex] := AFieldName + AOrder;
      end;
      ApplySortToQuery(
        TQuery(TcxGridDBDataController(DataController).DataSet), ASortArray);
    finally
      EndUpdate;
    end;
  end;
var
 AGridView: TcxCustomGridTableView;
begin
  try
    Screen.Cursor := crHourGlass;
    AGridView := TcxGridDBDataController(Sender).GridView;
    if AGridView.IsMaster or AGridView.IsPattern then
      SortPattern
    else
      SortClone;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TGridModeDemoMainForm.tvOrdersDataControllerSummaryAfterSummary(
  ASender: TcxDataSummary);

  function SummaryKindToStr(AKind: TcxSummaryKind): string;
  begin
    case AKind of
      skSum:
        Result := 'SUM';
      skMin:
        Result := 'MIN';
      skMax:
        Result := 'MAX';
      skCount:
        Result := 'Count';
      skAverage:
        Result := 'AVG';
    else
      Result := ''
    end;
  end;

var
  I: Integer;
  AStr, AFieldName: string;
  ADataController: TcxGridDBDataController;
  AView: TcxGridDBTableView;
begin
  ADataController := TcxGridDBDataController(ASender.DataController);
  AView := TcxGridDBTableView(ADataController.GridView);
  if not AView.IsPattern and ADataController.dataSet.Active then
    with GridModeDemoDataDM do
      try
        Screen.Cursor := crHourGlass;
        AStr := 'Select ';
        for I := 0 to ASender.FooterSummaryItems.Count - 1 do
        begin
          AFieldName :=
            TcxGridDBTableSummaryItem(ASender.FooterSummaryItems[I]).FieldName;
          AStr := AStr + SummaryKindToStr(ASender.FooterSummaryItems[I].Kind)+
            '(' + AFieldName + '), ';
        end;
        Delete(AStr, Length(AStr)-1, 2);
        if not Assigned(ADataController.DataSet) then exit;
        if (ADataController.DataSet.State = dsBrowse) and
         TQuery(ADataController.DataSet).UpdatesPending then
          TQuery(ADataController.DataSet).ApplyUpdates;
        AStr := AStr + ' From ' + GetTableNameByDataSet(ADataController.DataSet);
        qryHelper.SQL.Clear;
        qryHelper.SQL.Add(AStr);
        qryHelper.SQL.Add(GetSQLCondition(AView, True));
        qryHelper.Open;
        qryHelper.First;
        for I := 0 to ASender.FooterSummaryItems.Count - 1 do
          ASender.FooterSummaryValues[I] := qryHelper.Fields[I].Value;
        qryHelper.Close;
      finally
        Screen.Cursor := crDefault;
      end;
end;

procedure TGridModeDemoMainForm.btnPopulateClick(Sender: TObject);
begin
  if MessageDlg('This operation will take some time, continue?',
    mtWarning, [mbYES,mbNO], 0) = mrYes then
  begin
    tblOrdersPopulate;
    btnPopulate.Enabled := False;
  end;
end;

procedure TGridModeDemoMainForm.tblOrdersPopulate;

  procedure FillList(AList: TList; ADataSet: TDataSet; AKeyFieldName: string);
  var
    ABookmark: TcxBookmark;
    AKeyValue: Integer;
  begin
    try
      ADataSet.DisableControls;
      ABookmark := ADataSet.Bookmark;
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        AKeyValue := Integer(ADataSet.FindField(AKeyFieldName).Value);
        AList.Add(TObject(AKeyValue));
        ADataSet.Next;
      end;
    finally
      ADataSet.Bookmark := ABookmark;
      ADataSet.EnableControls;
    end;
  end;

const
  APaymentTypes: array[0..3] of string = ('AmEx','Cash','Visa','Master');
var
  ACarList, ACustomersList: TList;
  I, ARecordNo: Integer;
  ADateTime: TDateTime;
begin
  ACarList := TList.Create;
  ACustomersList := TList.Create;
  with GridModeDemoDataDM do
  begin
    FillList(ACarList, qryCars, 'ID');
    FillList(ACustomersList, qryCustomers, 'ID');
    try
      Screen.Cursor := crHourGlass;
      qryOrders.DisableControls;
      qryOrders.Close;
      qryHelper.SQL.Clear;
      qryHelper.SQL.Add('Select * from orders');
      qryHelper.Open;
      qryHelper.Last;
      FStartIDValue := qryHelper.FindField('ID').Value;
      ADateTime := Date;
      Randomize;
      ProgressBar.Position := 0;
      pnlProgress.Visible := True;
      Application.ProcessMessages;
      for I := 0 to 100000 do
      begin
        if (I > 0) and (I mod 100 = 0) then
        begin
          ProgressBar.Position := ProgressBar.Position + 1;
          Application.ProcessMessages;
        end;
        qryHelper.Append;
        ARecordNo := Random(ACustomersList.Count);
        qryHelper.FindField('CustomerID').Value :=
          Integer(ACustomersList[ARecordNo]);
        ARecordNo := Random(ACarList.Count);
        qryHelper.FindField('ProductID').Value := Integer(ACarList[ARecordNo]);
        qryHelper.FindField('PurchaseDate').Value := ADateTime - Random(1095);
        qryHelper.FindField('PaymentType').Value := APaymentTypes[Random(4)];
        qryHelper.FindField('PaymentAmount').Value := 20000 + Random(500000);
        qryHelper.Post;
      end;
      pnlProgress.Visible := False;
    finally
      FEndIDValue := qryHelper.FindField('ID').Value;
      qryHelper.Close;
      qryOrders.Open;
      qryOrders.EnableControls;
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TGridModeDemoMainForm.GetSQLCondition(
  AView: TcxGridDBTableView; AAddFilter: Boolean): string;
var
  AFieldName, AMasterKeyFieldName: string;
  AMasterGridView: TcxGridDBTableView;
  ADataController: TcxGridDBDataController;
  AMasterDataSet: TDataSet;
  AMasterKeyValue: Variant;
  AStr: string;
begin
  Result := '';
  AStr := 'where ';
  ADataController := AView.DataController;
  if AView.IsDetail then
  begin
    AStr := ' and ';
    AFieldName := ADataController.DetailKeyFieldNames;
    AMasterKeyFieldName := ADataController.MasterKeyFieldNames;
    AMasterGridView := TcxGridDBTableView(AView.MasterGridView);
    AMasterDataSet := AMasterGridView.DataController.DataSet;
    AMasterKeyValue := AMasterDataSet.FindField(AMasterKeyFieldName).Value;
    Result := 'where ' + AFieldName;
    if VarIsNull(AMasterKeyValue) then
      Result := Result + ' is NULL'
    else
      Result := Result + ' = ' + VarToStr(AMasterKeyValue);
  end;
  if AAddFilter and not ADataController.Filter.IsEmpty and ADataController.Filter.Active then
  begin
    Result := Result + AStr + ADataController.Filter.FilterText;
  end;
end;

procedure TGridModeDemoMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FStartIDValue <> FEndIDValue then
    with GridModeDemoDataDM do
    begin
      if not (MessageDlg('Do you want to delete previously inserted records?',
        mtConfirmation, [mbYes,mbNo], 0) = mrYes) then Exit;
      GridModeDemoTerminateForm.lbDesc.Caption := strDeleting;
      GridModeDemoTerminateForm.Show;
      try
        Screen.Cursor := crHourGlass;
        Application.ProcessMessages;
        qryHelper.SQL.Clear;
        qryHelper.SQL.Add('delete from Orders ');
        qryHelper.SQL.Add('where ID > ' + IntToStr(FStartIDValue));
        qryHelper.SQL.Add('and ID <= ' + IntToStr(FEndIDValue));
        qryHelper.ExecSQL;
      finally
        GridModeDemoTerminateForm.Close;
        Screen.Cursor := crDefault;
      end;
    end;
end;

procedure TGridModeDemoMainForm.FormCreate(Sender: TObject);
begin
  try
    GridModeDemoTerminateForm := TGridModeDemoTerminateForm.Create(nil);
    GridModeDemoTerminateForm.lbDesc.Caption := strLoadData;
    GridModeDemoTerminateForm.Show;
    Application.ProcessMessages;
    CopyDBToLocalPlace(False);
    OpenTables(True);
    InitPopulateButton;
  finally
    GridModeDemoTerminateForm.Close;
  end;
end;

procedure TGridModeDemoMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(GridModeDemoTerminateForm);
end;

procedure TGridModeDemoMainForm.OpenTables(AOpen: Boolean);
begin
  with GridModeDemoDataDM do
  try
    Screen.Cursor := crHourGlass;
    DataBase.Connected := False;
    qryCars.Active := AOpen;
    qryOrders.Active := AOpen;
    qryCustomers.Active := AOpen;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TGridModeDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvCars);
  UpdateTableViewStyleSheet(tvOrders);
end;

procedure TGridModeDemoMainForm.CopyDBToLocalPlace(ARewrite: Boolean);

  function CopyTableToCurrentDir(ATableName: string): Boolean;
   var
     ASearchRec: TSearchRec;
  begin
    Result := False;
    if FindFirst(DataPath + ATableName + '.*', faAnyFile, ASearchRec) = 0 then
    begin
      Result :=
        CopyFile({$IFNDEF CLR}PChar{$ENDIF}(DataPath + ASearchRec.Name),
          {$IFNDEF CLR}PChar{$ENDIF}(ASearchRec.Name), False);
      while FindNext(ASearchRec) = 0 do
        Result := Result and
          CopyFile({$IFNDEF CLR}PChar{$ENDIF}(DataPath + ASearchRec.Name),
            {$IFNDEF CLR}PChar{$ENDIF}(ASearchRec.Name), False);
      FindClose(ASearchRec);
    end;
  end;

var
 I: Integer;
 AEnablePopulate: Boolean;
begin
  try
    Screen.Cursor := crHourGlass;
    OpenTables(False);
    AEnablePopulate := True;
    for I := Low(CarsTableNames) to High(CarsTableNames) do
      if ARewrite or not FileExists(CarsTableNames[I] + '.DB') then
        AEnablePopulate := AEnablePopulate and
          CopyTableToCurrentDir(CarsTableNames[I]);
  finally
    OpenTables(True);
    Screen.Cursor := crDefault;
  end;
  if AEnablePopulate then
  begin
    btnPopulate.Enabled := True;
    FStartIDValue := 0;
    FEndIDValue := 0;
  end;
end;

procedure TGridModeDemoMainForm.InitPopulateButton;
begin
  with GridModeDemoDataDM do
  begin
    qryHelper.SQL.Clear;
    qryHelper.SQL.Add('Select Count(ID) from Orders');
    qryHelper.Open;
    btnPopulate.Enabled := qryHelper.Fields[0].AsInteger < 100000;
    qryHelper.Close;
  end;
end;

end.
 
