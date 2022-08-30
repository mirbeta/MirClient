{
  @html(<b>)
  RTC Master Detail Client Demo Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This Project shows how to write a RTC Client using DB-aware components with
  DataSets in Master/Detail relations. This Client is designed to work together
  with the "BDEDemoServer" Project for remote access the the "DBDEMOS" Database.

  This Client version uses asynchronous communication with the TRtcMemDataSet component.
}
unit MasterDetailCli;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, DBGrids, DB,
  StdCtrls, Buttons, DBCtrls, ComCtrls,

  rtcConn, rtcDataCli, rtcHttpCli,
  rtcCliModule, rtcFunction,
  rtcInfo, rtcDB;

type
  TForm1 = class(TForm)
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnRefreshDataSet1: TSpeedButton;
    eAddr: TEdit;
    ePort: TEdit;
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    RtcResult1: TRtcResult;
    RtcMemDataSet1: TRtcMemDataSet;
    DataSource1: TDataSource;
    RtcMemDataSet2: TRtcMemDataSet;
    DataSource2: TDataSource;
    Panel1: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    DBNavigator1: TDBNavigator;
    Label3: TLabel;
    RtcMemDataSet3: TRtcMemDataSet;
    DataSource3: TDataSource;
    DBGrid1: TDBGrid;
    Panel8: TPanel;
    lStatus: TLabel;
    Splitter3: TSplitter;
    Panel3: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    Label5: TLabel;
    DBNavigator3: TDBNavigator;
    DBGrid3: TDBGrid;
    Panel2: TPanel;
    Panel6: TPanel;
    Label4: TLabel;
    DBNavigator2: TDBNavigator;
    DBGrid2: TDBGrid;
    Splitter2: TSplitter;
    xValidCheck: TCheckBox;
    Panel10: TPanel;
    btnRefreshDataSet2: TSpeedButton;
    Panel11: TPanel;
    btnRefreshDataSet3: TSpeedButton;
    procedure eAddrChange(Sender: TObject);
    procedure RtcResult1Return(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure btnRefreshDataSet1Click(Sender: TObject);
    procedure RtcMemDataSetDataChange(Sender: TObject);
    procedure btnRefreshDataSet2Click(Sender: TObject);
    procedure btnRefreshDataSet3Click(Sender: TObject);
    procedure RtcMemDataSet2NewRecord(DataSet: TDataSet);
    procedure RtcMemDataSet3NewRecord(DataSet: TDataSet);
    procedure RtcMemDataSet1Scrolled(DataSet: TDataSet);
    procedure RtcMemDataSet2Scrolled(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure RtcResult1RequestAborted(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure RtcMemDataSet2BeforePost(DataSet: TDataSet);
    procedure RtcMemDataSet1BeforePost(DataSet: TDataSet);
    procedure RtcMemDataSet3BeforePost(DataSet: TDataSet);
    procedure RtcMemDataSet2BeforeDelete(DataSet: TDataSet);
    procedure RtcMemDataSet1BeforeDelete(DataSet: TDataSet);
    procedure RtcResult1PreparingCall(Sender: TRtcConnection; Data,
      Result: TRtcValue);
  private
    { Private declarations }
  public
    { Public declarations }
    CustNo,OrderNo:integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  CustNo:=0; // Currently selected Customer
  OrderNo:=0; // Currently selected Order
  end;

procedure TForm1.eAddrChange(Sender: TObject);
  begin
  lStatus.Caption:='Changing Server Address and Port to '+eAddr.Text+':'+ePort.Text;
  RtcHttpClient1.Disconnect;
  RtcHttpClient1.ServerAddr:=eAddr.Text;
  RtcHttpClient1.ServerPort:=ePort.Text;
  end;

procedure TForm1.btnRefreshDataSet1Click(Sender: TObject);
  begin
  { Close Orders and Items DataSets, so the user can't make changes
    to unrelated Orders or Items while waiting for new Customer data ... }
  CustNo:=0;
  RtcMemDataSet2.Active:=False;
  OrderNo:=0;
  RtcMemDataSet3.Active:=False;

  { Closing the DataSet for which new data is being requested is NOT
    necessary, but it shows the user that new data is being loaded. }
  RtcMemDataSet1.Active:=False;
  lStatus.Caption:='Requesting Customer data';

  RtcClientModule1.Prepare('select');
  RtcClientModule1.Param.asText['table']:='customer';
  RtcClientModule1.Call(RtcResult1, not assigned(Sender));
  end;

procedure TForm1.btnRefreshDataSet2Click(Sender: TObject);
  begin
  { Close the Items DataSet, so the user can't make changes
    to unrelated Items while waiting for new Order data ... }
  OrderNo:=0;
  RtcMemDataSet3.Active:=False;

  if CustNo<>0 then
    begin
    { Closing the DataSet for which new data is being requested is NOT
      necessary, but it shows the user that new data is being loaded. }
    RtcMemDataSet2.Active:=False;
    lStatus.Caption:='Requesting Orders data, CustNo='+IntToStr(CustNo);

    RtcClientModule1.Prepare('select');
    RtcClientModule1.Param.asText['table']:='orders';
    RtcClientModule1.Param.NewRecord('eq').asInteger['CustNo']:=CustNo;
    RtcClientModule1.Call(RtcResult1);
    end
  else
    begin
    { There is no Customer info, close the Orders dataset. }
    RtcMemDataSet2.Active:=False;
    lStatus.Caption:='No Customer selected, Orders table closed.';
    end;
  end;

procedure TForm1.btnRefreshDataSet3Click(Sender: TObject);
  begin
  if OrderNo<>0 then
    begin
    { Closing the DataSet for which new data is being requested is NOT
      necessary, but it shows the user that new data is being loaded. }
    RtcMemDataSet3.Active:=False;
    lStatus.Caption:='Requesting Items data, OrderNo='+IntToStr(OrderNo);

    RtcClientModule1.Prepare('select');
    RtcClientModule1.Param.asText['table']:='items';
    RtcClientModule1.Param.NewRecord('eq').asInteger['OrderNo']:=OrderNo;
    RtcClientModule1.Call(RtcResult1);
    end
  else
    begin
    { There is no Order info, close the Items dataset. }
    RtcMemDataSet3.Active:=False;
    lStatus.Caption:='No Order selected, Items table closed.';
    end;
  end;

procedure TForm1.RtcMemDataSetDataChange(Sender: TObject);
  var
    data:TRtcValue;
    tblName:String;
  begin
  { We are using a single "OnDataChange" implementation for all DataSets }
  if Sender=RtcMemDataSet1 then
    begin
    tblName:='customer';
    // Refresh Orders if Customer field value has changed
    RtcMemDataSet1Scrolled(nil);
    end
  else if Sender=RtcMemDataSet2 then
    begin
    tblName:='orders';
    // Refresh Items if Order field value was changed
    RtcMemDataSet2Scrolled(nil);
    end
  else if Sender=RtcMemDataSet3 then
    tblName:='items';

  data:=TRtcMemDataSet(Sender).ExtractChanges;
  if assigned(data) then
    begin
    lStatus.Caption:='Sending '+tblName+' changes to the Server ...';
    RtcClientModule1.Prepare('submit');
    RtcClientModule1.Param.asText['table']:=tblName;
    RtcClientModule1.Param.asObject['change_data']:=data;
    RtcClientModule1.Call(RtcResult1);
    end;
  end;

procedure TForm1.RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
  var
    tblName:String;
    dts:TRtcMemDataSet;
  begin
  tblName:=Data.asFunction.asText['table'];
  if Data.asFunction.FunctionName='select' then
    begin
    if Result.isType=rtc_DataSet then
      begin
      if tblName='customer' then
        begin
        dts:=RtcMemDataSet1;

        lStatus.Caption:='Received Customer data';
        end
      else if tblName='orders' then
        begin
        dts:=RtcMemDataSet2;

        lStatus.Caption:='Received Orders data, CustNo='+
                          Data.asFunction.asRecord['eq'].asText['CustNo'];
        { Because our requests are sent asynchronously,
          it is possible to receive a result for a master row
          after moving to a new master row. In that case,
          we should simply ignore the result. }
        if Data.asFunction.asRecord['eq'].asInteger['CustNo']<>CustNo then
          begin
          lStatus.Caption:=lStatus.Caption+', old (ignored).';
          Exit;
          end;
        end
      else if tblName='items' then
        begin
        dts:=RtcMemDataSet3;

        lStatus.Caption:='Received Items data, OrderNo='+
                          Data.asFunction.asRecord['eq'].asText['OrderNo'];
        { Because our requests are sent asynchronously,
          it is possible to receive a result for a master row
          after moving to a new master row. In that case,
          we should simply ignore the result. }
        if Data.asFunction.asRecord['eq'].asInteger['OrderNo']<>OrderNo then
          begin
          lStatus.Caption:=lStatus.Caption+', old (ignored).';
          Exit;
          end;
        end
      else
        Exit; // Exit here to avoid a warning about undefined "dts" from the compiler (should never happen)

      dts.asObject:=Result.asDataSet;
      Result.Extract;
      dts.Active:=True;
      end
    else
      begin
      if Result.isType=rtc_Exception then
        ShowMessage('Server-side exception after '+tblName+' Select:'+#13#10+Result.asException)
      else
        ShowMessage('Unexpected Result after '+tblName+' Select:'#13#10+Result.asCode);
      end;
    end
  else if Data.asFunction.FunctionName='submit' then
    begin
    if (Result.isType<>rtc_Boolean) or (Result.asBoolean<>TRUE) then
      begin
      if Result.isType=rtc_Exception then
        ShowMessage('Server-side exception after '+tblName+' Submit:'+#13#10+Result.asException)
      else
        ShowMessage('Unexpected Result after '+tblName+' Submit:'+#13#10+Result.asCode);
      end
    else
      lStatus.Caption:='Changes to '+tblName+' submitted.';
    end;
  end;

procedure TForm1.RtcResult1RequestAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Data.asFunction.FunctionName='select' then
    lStatus.Caption:='Communication problems while sending a SELECT request to the Server'
  else if Data.asFunction.FunctionName='submit' then
    lStatus.Caption:='Communication problems while submitting changes to the Server';
  end;

procedure TForm1.RtcMemDataSet1Scrolled(DataSet: TDataSet);
  var
    NewNo:integer;
  begin
  { Customer data received, or the user has scrolled to a new Customer row.
    For our Master/Detail relation with Orders to work,
    we need to refresh our "Orders" data with a new "CustNo". }

  if RtcMemDataSet1.Active and (RtcMemDataSet1.RecordCount>0) then
    NewNo:=RtcMemDataSet1.FieldByName('CustNo').AsInteger
  else
    NewNo:=0;

  if CustNo<>NewNo then
    begin
    CustNo:=NewNo;
    btnRefreshDataSet2Click(nil);
    end;
  end;

procedure TForm1.RtcMemDataSet2Scrolled(DataSet: TDataSet);
  var
    NewNo:integer;
  begin
  { Orders data received, or the user has scrolled to a new Order row.
    For our Master/Detail relation with Items to work,
    we need to refresh our "Items" data with a new "OrderNo". }

  if RtcMemDataSet2.Active and (RtcMemDataSet2.RecordCount>0) then
    NewNo:=RtcMemDataSet2.FieldByName('OrderNo').AsInteger
  else
    NewNo:=0;

  if OrderNo<>NewNo then
    begin
    OrderNo:=NewNo;
    btnRefreshDataSet3Click(nil);
    end;
  end;

procedure TForm1.RtcMemDataSet2NewRecord(DataSet: TDataSet);
  begin
  { We should set all "master" fields for a new Order here }
  RtcMemDataSet2.FieldByName('CustNo').AsInteger:=CustNo;
  end;

procedure TForm1.RtcMemDataSet3NewRecord(DataSet: TDataSet);
  begin
  { We should set all "master" fields for a new Item here }
  RtcMemDataSet3.FieldByName('OrderNo').AsInteger:=OrderNo;
  end;

procedure TForm1.RtcMemDataSet1BeforePost(DataSet: TDataSet);
  begin
  if not xValidCheck.Checked then Exit;

  { We can leave this job to the Server, but then the user will
    have to refresh the dataset and repeat the last operation in
    case a field was undefined, so we will make this check here. }
  if RtcMemDataSet1.FieldByName('CustNo').IsNull then
    raise Exception.Create('Required field "CustNo" is NULL');
  end;

procedure TForm1.RtcMemDataSet2BeforePost(DataSet: TDataSet);
  begin
  if not xValidCheck.Checked then Exit;

  { We can leave this job to the Server, but then the user will
    need to refresh the dataset and repeat the last operation in
    case a field is undefined, so we will make this check here. }
  if RtcMemDataSet2.FieldByName('CustNo').AsInteger<>CustNo then
    raise Exception.Create('Required field "CustNo" has invalid value')
  else if RtcMemDataSet2.FieldByName('OrderNo').IsNull then
    raise Exception.Create('Required field "OrderNo" is NULL')
  else if RtcMemDataSet2.FieldByName('EmpNo').IsNull then
    raise Exception.Create('Required field "EmpNo" is NULL');
  end;

procedure TForm1.RtcMemDataSet3BeforePost(DataSet: TDataSet);
  begin
  if not xValidCheck.Checked then Exit;

  { We can leave this job to the Server, but then the user will
    need to refresh the dataset and repeat the last operation in
    case a field is undefined, so we will make this check here. }
  if RtcMemDataSet3.FieldByName('OrderNo').AsInteger<>OrderNo then
    raise Exception.Create('Required field "OrderNo" has invalid value')
  else if RtcMemDataSet3.FieldByName('ItemNo').IsNull then
    raise Exception.Create('Required field "ItemNo" is NULL');
  end;

procedure TForm1.RtcMemDataSet2BeforeDelete(DataSet: TDataSet);
  begin
  if not xValidCheck.Checked then Exit;

  { We can leave this job to the Server, but then the user will see
    the record deleted locally and then get an error from the Server. }
  if RtcMemDataSet3.RecordCount>0 then
    raise Exception.Create('Can not delete Order before deleting all Order Items.');
  end;

procedure TForm1.RtcMemDataSet1BeforeDelete(DataSet: TDataSet);
  begin
  if not xValidCheck.Checked then Exit;

  { We can leave this job to the Server, but then the user will see
    the record deleted locally and then get an error from the Server. }
  if RtcMemDataSet2.RecordCount>0 then
    raise Exception.Create('Can not delete Customer before deleting all Customer Orders.');
  end;

procedure TForm1.RtcResult1PreparingCall(Sender: TRtcConnection; Data, Result: TRtcValue);
  var
    tblName:String;
  begin
  // Here, we can notify the user about each remote call just BEFORE it is sent to the Server,
  // and ... we can SKIP remote calls which are no longer relevant to the Client.
  tblName:=Data.asFunction.asText['table'];
  if Data.asFunction.FunctionName='select' then
    begin
    if tblName='customer' then
      lStatus.Caption:='Requesting Customer data'
    else if tblName='orders' then
      begin
      if Data.asFunction.asRecord['eq'].asInteger['CustNo']<>CustNo then
        Data.isNull:=True // skip the call, it was made for a Customer we already moved away from
      else
        lStatus.Caption:='Requesting Orders data, CustNo='+
                          Data.asFunction.asRecord['eq'].asText['CustNo'];
      end
    else if tblName='items' then
      begin
      if Data.asFunction.asRecord['eq'].asInteger['OrderNo']<>OrderNo then
        Data.isNull:=True // skip the call, it was made for the Order we already moved away from
      else
        lStatus.Caption:='Requesting Items data, OrderNo='+
                          Data.asFunction.asRecord['eq'].asText['OrderNo'];
      end;
    end
  else if Data.asFunction.FunctionName='submit' then
    lStatus.Caption:='Submitting changes for '+tblName;
  end;

end.
