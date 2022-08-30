{=======================================================================================================================
  CustomerForm Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
==================================================y=====================================================================}

{$I RCDemo.inc}

unit CustomerForm;

interface

uses
  Forms,
  DB,
  DBClient,
  RzCommon,
  ExtCtrls,
  DBCtrls,
  RzDBNav,
  Controls,
  RzButton,
  Mask,
  RzEdit,
  RzDBEdit,
  StdCtrls,
  RzCmboBx,
  RzDBCmbo,
  RzDBLbl,
  RzLabel,
  Graphics,
  Classes,
  RzBckgnd, RzBorder, RzPanel;

type
  TFrmCustomer = class(TForm)
    srcCustomers: TDataSource;
    RzFrameController1: TRzFrameController;
    RzLabel2: TRzLabel;
    RzLabel3: TRzLabel;
    RzLabel4: TRzLabel;
    RzLabel6: TRzLabel;
    RzLabel7: TRzLabel;
    RzLabel1: TRzLabel;
    RzLabel10: TRzLabel;
    RzLabel11: TRzLabel;
    RzLabel12: TRzLabel;
    RzLabel13: TRzLabel;
    RzLabel14: TRzLabel;
    RzLabel15: TRzLabel;
    RzLabel9: TRzLabel;
    EdtFirstName: TRzDBEdit;
    EdtLastName: TRzDBEdit;
    EdtAddress1: TRzDBEdit;
    EdtCity: TRzDBEdit;
    EdtPostalCode: TRzDBEdit;
    CbxState: TRzDBComboBox;
    EdtAddress2: TRzDBEdit;
    EdtCountry: TRzDBEdit;
    EdtPhone: TRzDBEdit;
    EdtFax: TRzDBEdit;
    EdtEmail: TRzDBEdit;
    EdtRemarks: TRzDBMemo;
    CbxVIPStatus: TRzDBComboBox;
    EdtCustNo: TRzDBEdit;
    RzDBLabel1: TRzDBLabel;
    cdsCustomers: TClientDataSet;
    RzPanel1: TRzPanel;
    RzPanel2: TRzPanel;
    RzDBNavigator1: TRzDBNavigator;
    RzButton1: TRzButton;
    RzButton2: TRzButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
  MainForm;

procedure TFrmCustomer.FormCreate(Sender: TObject);
begin
  cdsCustomers.LoadFromFile( 'Customers.cds' );
  cdsCustomers.Open;
end;

procedure TFrmCustomer.FormActivate(Sender: TObject);
begin
  FrmMain.LoadNotes( 'Customer' );
end;


end.



