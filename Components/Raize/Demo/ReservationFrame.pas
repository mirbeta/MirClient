{=======================================================================================================================
  ReservationFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit ReservationFrame;

interface

uses
  Forms,
  Windows,
  DB,
  DBClient,
  RzCommon,
  RzDBLook,
  DBCtrls,
  RzDBNav,
  RzDBEdit,
  Controls,
  StdCtrls,
  RzCmboBx,
  RzDBCmbo,
  RzButton,
  RzRadChk,
  RzDBChk,
  RzDBSpin,
  Mask,
  RzEdit,
  RzDBBnEd,
  RzLabel,
  RzPanel,
  RzStatus,
  RzDBStat,
  Classes,
  ExtCtrls,
  Grids,
  DBGrids,
  RzBorder, RzLine, RzDBGrid, ImgList;

type
  TFmeReservations = class(TFrame)
    srcReservations: TDataSource;
    srcEvents: TDataSource;
    RzStatusBar1: TRzStatusBar;
    RzDBStateStatus1: TRzDBStateStatus;
    srcCustomers: TDataSource;
    dlgLookupEvent: TRzDBLookupDialog;
    stsResNo: TRzDBStatusPane;
    RzFrameController1: TRzFrameController;
    pnlTicket: TRzPanel;
    RzLabel9: TRzLabel;
    RzLabel4: TRzLabel;
    RzLabel1: TRzLabel;
    edtCustomer: TRzDBButtonEdit;
    spnTickets: TRzDBSpinEdit;
    edtEvent: TRzDBButtonEdit;
    RzLabel5: TRzLabel;
    RzLabel6: TRzLabel;
    RzLabel10: TRzLabel;
    RzLabel11: TRzLabel;
    RzLabel12: TRzLabel;
    edtNotes: TRzDBMemo;
    edtAmount: TRzDBEdit;
    chkPaid: TRzDBCheckBox;
    edtCardNumber: TRzDBEdit;
    cbxMethod: TRzDBComboBox;
    stsEventName: TRzDBStatusPane;
    RzClockStatus: TRzClockStatus;
    RzLabel2: TRzLabel;
    edtExpirationDate: TRzDBDateTimeEdit;
    cdsReservations: TClientDataSet;
    cdsEvents: TClientDataSet;
    cdsCustomers: TClientDataSet;
    cdsEventLookup: TClientDataSet;
    cdsReservationsResNo: TAutoIncField;
    cdsReservationsEventNo: TIntegerField;
    cdsReservationsCustNo: TIntegerField;
    cdsReservationsNumTickets: TIntegerField;
    cdsReservationsAmt_Paid: TCurrencyField;
    cdsReservationsPay_Method: TStringField;
    cdsReservationsCard_No: TStringField;
    cdsReservationsCard_Exp: TDateField;
    cdsReservationsPay_Notes: TMemoField;
    cdsReservationsPurge_Date: TDateField;
    cdsReservationsPaid: TBooleanField;
    cdsReservationsCustName: TStringField;
    cdsReservationsEventName: TStringField;
    cdsEventLookupEventNo: TAutoIncField;
    cdsEventLookupVenueNo: TIntegerField;
    cdsEventLookupEvent_Name: TStringField;
    cdsEventLookupEvent_Date: TDateField;
    cdsEventLookupEvent_Time: TTimeField;
    cdsEventLookupTicket_price: TCurrencyField;
    RzPanel1: TRzPanel;
    pnlHeader: TRzPanel;
    RzPanel2: TRzPanel;
    navReservations: TRzDBNavigator;
    RzLabel3: TRzLabel;
    grdReservations: TRzDBGrid;
    procedure edtEventButtonClick(Sender: TObject);
    procedure edtCustomerButtonClick(Sender: TObject);
    procedure dlgLookupEventDrawDataCell(Grid: TDBGrid; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure cdsReservationsCalcFields(DataSet: TDataSet);
    procedure RzPanel1Paint(Sender: TObject);
  private
  published
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}

uses
  Graphics,
  MainForm,
  CustomerForm;


procedure TFmeReservations.Init;
begin
//  ParentBackground := False;

  CdsEvents.Open;
  CdsEventLookup.LoadFromFile( 'Events.cds' );   // Load from file b/c do not want changes to be persistent
  CdsEventLookup.Open;
  CdsCustomers.Open;
  CdsReservations.Open;  // Must open last b/c of calculated fields
end;


procedure TFmeReservations.UpdateVisualStyle( VS: TRzVisualStyle;
                                              GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
end;


procedure TFmeReservations.cdsReservationsCalcFields(DataSet: TDataSet);
begin
  CdsCustomers.Locate( 'CustNo', CdsReservationsCustNo.Value, [] );
  CdsReservationsCustName.Value := AnsiString( CdsCustomers.FieldByName( 'First_Name' ).AsString + ' ' +
                                               CdsCustomers.FieldByName( 'Last_Name' ).AsString );

  CdsEvents.Locate( 'EventNo', CdsReservationsEventNo.Value, [] );
  CdsReservationsEventName.Value := AnsiString( CdsEvents.FieldByName( 'Event_Name' ).AsString );
end;



procedure TFmeReservations.edtEventButtonClick(Sender: TObject);
begin
  DlgLookupEvent.Execute;
end;


procedure TFmeReservations.edtCustomerButtonClick(Sender: TObject);
var
  F: TFrmCustomer;
begin
  F := TFrmCustomer.Create( Application );
  try
    F.cdsCustomers.Locate( 'CustNo', cdsReservationsCustNo.Value, [] );

    if F.ShowModal = mrOK then
    begin
      if not ( cdsReservations.State in dsEditModes ) then
        cdsReservations.Edit;
      cdsReservationsCustNo.Value := F.cdsCustomers.FieldByName( 'CustNo' ).AsInteger;
    end;
  finally
    F.Free;
    FrmMain.LoadNotes( 'Reservation' );
  end;
end;


procedure TFmeReservations.dlgLookupEventDrawDataCell(Grid: TDBGrid;
  const Rect: TRect; Field: TField; State: TGridDrawState);
begin
  if Field.Dataset.FieldByName( 'Ticket_price' ).AsFloat > 10.00 then
    Grid.Canvas.Font.Color := clRed;
  Grid.DefaultDrawDataCell( Rect, Field, State );
end;


procedure TFmeReservations.RzPanel1Paint(Sender: TObject);
begin
  DrawDropShadow( RzPanel1.Canvas, pnlTicket.BoundsRect, 6 );
  DrawDropShadow( RzPanel1.Canvas, RzPanel2.BoundsRect, 6 );
  DrawDropShadow( RzPanel1.Canvas, grdReservations.BoundsRect, 6 );
end;

end.
