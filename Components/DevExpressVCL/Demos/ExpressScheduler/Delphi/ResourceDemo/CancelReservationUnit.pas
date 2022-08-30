unit CancelReservationUnit;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxLookAndFeelPainters, StdCtrls, cxButtons, cxListBox, ExtCtrls,
  cxControls, cxContainer, cxEdit, cxGroupBox, cxSchedulerStorage, cxLabel;

type
  TfrmCancelReservation = class(TForm)
    lbxCustomers: TcxListBox;
    lbxEvents: TcxListBox;
    lbSelectCustomer: TcxLabel;
    lbCancelReservation: TcxLabel;
    btnCancelReserv: TcxButton;
    btnClose: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure lbxCustomersClick(Sender: TObject);
    procedure lbxEventsClick(Sender: TObject);
    procedure btnCancelReservClick(Sender: TObject);
  private
    function Storage: TcxCustomSchedulerStorage;
  public
    Date: TDateTime;
    Index: Integer;
    procedure CheckButtonEnabled;
    procedure FillCustomersList;
    procedure FillEventsList(ACustomerIndex: Integer);
  end;

implementation

uses ResourceMainUnit, cxDateUtils;

{$R *.dfm}

function EventsCompare(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  if AEvent1.Start = AEvent2.Start then
    Result := 0
  else
    if AEvent1.Start < AEvent2.Start then
      Result := -1
    else
      Result := 1;
end;

procedure TfrmCancelReservation.CheckButtonEnabled;
begin
  btnCancelReserv.Enabled := lbxEvents.SelCount > 0;
end;

procedure TfrmCancelReservation.FillCustomersList;
var
  I: Integer;
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    for I := 0 to Storage.EventCount - 1 do
      if Storage.Events[I].EventType = etNone then
        AList.Add(Storage.Events[I].Caption);
    AList.Sort;
    lbxCustomers.Items.BeginUpdate;
    try
      lbxCustomers.Items.Clear;
      for I := 0 to AList.Count - 1 do
        if (I = 0) or (AList[I - 1] <> AList[I]) then
          lbxCustomers.Items.Add(AList[I]);
    finally
      lbxCustomers.Items.EndUpdate;
      if lbxCustomers.Count > 0 then
        lbxCustomers.ItemIndex := 0;
      FillEventsList(lbxCustomers.ItemIndex);
    end;
  finally
    AList.Free;
  end;
  CheckButtonEnabled;
end;

procedure TfrmCancelReservation.FillEventsList(ACustomerIndex: Integer);
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
  AList: TcxSchedulerEventList;
begin
  with lbxEvents.Items do
  begin
    BeginUpdate;
    AList := TcxSchedulerEventList.Create;
    try
      Clear;
      if ACustomerIndex >= 0 then
        for I := 0 to Storage.EventCount - 1 do
        begin
          AEvent := Storage.Events[I];
          if (AEvent.Caption = lbxCustomers.Items[ACustomerIndex]) and (AEvent.EventType = etNone)  then
            AList.Add(AEvent);
        end;
        AList.Sort(EventsCompare);
        for I := 0 to AList.Count - 1 do
        begin
          AEvent := AList[I];
          lbxEvents.AddItem(DateTimeToStr(AEvent.Start) + ' - ' + DateTimeToStr(AEvent.Finish), AEvent);
        end;
    finally
      AList.Free;
      EndUpdate;
      if lbxEvents.Count > 0 then
        lbxEvents.ItemIndex := 0;
    end;
  end;
  CheckButtonEnabled;
end;

function TfrmCancelReservation.Storage: TcxCustomSchedulerStorage;
begin
  Result := ResourceDemoMainForm.Storage;
end;

procedure TfrmCancelReservation.FormCreate(Sender: TObject);
begin
  Index := -1;
  Date := NullDate;
  FillCustomersList;
end;

procedure TfrmCancelReservation.lbxCustomersClick(Sender: TObject);
begin
  FillEventsList(lbxCustomers.ItemIndex);
end;

procedure TfrmCancelReservation.lbxEventsClick(Sender: TObject);
begin
  CheckButtonEnabled;
end;

procedure TfrmCancelReservation.btnCancelReservClick(Sender: TObject);
var
  I: Integer;
begin
  Storage.BeginUpdate;
  try
    for I := 0 to lbxEvents.Count - 1 do
      if lbxEvents.Selected[I] then
      begin
        with TcxSchedulerEvent(lbxEvents.Items.Objects[I]) do
        begin
          Self.Index := ResourceID;
          Date := Start;
          Delete;
        end;
      end;
  finally
    Storage.EndUpdate;
  end;
end;

end.
