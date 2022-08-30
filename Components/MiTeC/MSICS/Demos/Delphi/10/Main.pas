unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, MSI_APM, MiTeC_PowrProf, StdCtrls, ExtCtrls;

type
  TwndMain = class(TForm)
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Image6: TImage;
    eBatLife: TEdit;
    pbBat: TProgressBar;
    eBat: TEdit;
    TabSheet3: TTabSheet;
    Image1: TImage;
    cbCPU: TComboBox;
    Image2: TImage;
    eAPM: TEdit;
    eBatCap: TEdit;
    eBatRemain: TEdit;
    eBatRate: TEdit;
    Label41: TLabel;
    Label39: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    eFreq: TEdit;
    eMaxFreq: TEdit;
    eFreqLimit: TEdit;
    eMaxIdle: TEdit;
    eIdle: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Panel1: TPanel;
    Button2: TButton;
    pbCPU: TProgressBar;
    APMPanel: TPanel;
    cbxUPS: TCheckBox;
    cbxTC: TCheckBox;
    cbxVD: TCheckBox;
    cbxPT: TCheckBox;
    cbxDSD: TCheckBox;
    cbxPB: TCheckBox;
    cbxSB: TCheckBox;
    cbxLS: TCheckBox;
    cbxSysBat: TCheckBox;
    Label4: TLabel;
    eLST: TEdit;
    Label5: TLabel;
    eLWT: TEdit;
    Label6: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    eBatChem: TEdit;
    eBatMan: TEdit;
    eBatVolt: TEdit;
    Label14: TLabel;
    eBatID: TEdit;
    Panel2: TPanel;
    eAC: TEdit;
    Image3: TImage;
    cbBat: TComboBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cbCPUChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbBatChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    APM: TMiTeC_APM;
    procedure RefreshData;
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_Datetime, MiTeC_Routines;

{$R *.dfm}

procedure TwndMain.Button1Click(Sender: TObject);
begin
  refreshData;
end;

procedure TwndMain.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TwndMain.cbBatChange(Sender: TObject);
var
  s: string;
begin
  eBat.Text:='';
  eBatCap.Text:='';
  eBatRemain.Text:='';
  eBatRate.Text:='';
  eBatLife.Text:='';
  pbBat.Position:=0;

  if cbBat.ItemIndex=-1 then
    Exit;
  with APM.Battery[cbBat.ItemIndex] do begin
    if APM.BatteryChargeStatus<>[bsNoBattery] then begin
      s:='';
      if PowerState and BATTERY_CHARGING=BATTERY_CHARGING then
        s:=s+'charging, ';
      if PowerState and BATTERY_CRITICAL=BATTERY_CRITICAL then
        s:=s+'critical, ';
      if PowerState and BATTERY_DISCHARGING=BATTERY_DISCHARGING then
        s:=s+'discharging, ';
      if PowerState and BATTERY_POWER_ON_LINE=BATTERY_POWER_ON_LINE then
        s:=s+'power on-line, ';
      SetLength(s,Length(s)-2);
      eBat.Text:=s;
      eBatMan.Text:=Manufacturer;
      eBatChem.Text:=Chemistry;
      eBatID.Text:=UniqueID;
      eBatLife.Text:=FormatSeconds(EstimatedTime);
      if Capacity<>BATTERY_UNKNOWN_CAPACITY then
        eBatCap.Text:=Format('%d Wh',[Capacity div 1000])
      else
        eBatCap.Text:='?';
      if Voltage<>BATTERY_UNKNOWN_VOLTAGE then
        eBatVolt.Text:=Format('%d V',[Voltage div 1000])
      else
        eBatVolt.Text:='?';
      if CurrentCapacity<>BATTERY_UNKNOWN_CAPACITY then
        eBatRemain.Text:=Format('%d Wh',[CurrentCapacity div 1000])
      else
        eBatRemain.Text:='?';
      if Rate<>BATTERY_UNKNOWN_RATE then
        eBatRate.Text:=Format('%d W',[Rate div 1000])
      else
        eBatRate.Text:='?';
      pbBat.Max:=Capacity;
      pbBat.Position:=CurrentCapacity;
      cbBat.Hint:=SymbolicLink;
    end else
      eBat.Text:='No battery present';
  end;
end;

procedure TwndMain.cbCPUChange(Sender: TObject);
begin
  if cbCPU.ItemIndex=-1 then
    Exit;
  with APM.ProcessorPowerStatus[cbCPU.ItemIndex] do begin
    eMaxFreq.Text:=Format('%d MHz',[MaxMHz]);
    eFreq.Text:=Format('%d MHz',[CurrentMHz]);
    eFreqLimit.Text:=Format('%d MHz',[MHzLimit]);
    eMaxIdle.Text:=Format('%d',[MaxIdleState]);
    eIdle.Text:=Format('%d',[CurrentIdleState]);
    if MaxMHz>pbCPU.Max then
      pbCPU.Max:=MaxMHz;
    pbCPU.Position:=CurrentMHz;
  end;
end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  APM:=TMiTeC_APM.Create(Self);
  RefreshData;
end;

procedure TwndMain.RefreshData;
var
  s: string;
  i: Integer;
begin
  APM.RefreshData;
  if APM.BatteryCount=0 then
    pc.ActivePage:=Tabsheet2
  else
    pc.ActivePage:=Tabsheet1;
  Tabsheet1.TabVisible:=APM.BatteryCount>0;
  cbBat.Items.Clear;
  for i:=0 to APM.BatteryCount-1 do
    cbBat.Items.Add(Format('[%d] %s',[i+1,APM.Battery[i].Devicename]));
  cbBat.ItemIndex:=0;

  cbCPU.Items.Clear;
  for i:=0 to APM.ProcessorCount-1 do
    cbCPU.Items.Add(Format('Processor #%d',[APM.ProcessorPowerStatus[i].Number]));
  cbCPU.ItemIndex:=0;

  case APM.ACPowerStatus of
    psUnknown: s:='unknown power';
    psOnline: s:='AC power';
    psOffline: s:='Battery power';
  end;
  eAC.Text:=Format('System is running on %s',[s]);
  cbBatChange(nil);
  cbCPUChange(nil);

  {if APM.SystemPower.ApmPresent then
    eAPM.Text:='System supports APM'
  else
    eAPM.Text:='APM not supported';
  APMPanel.Visible:=APM.SystemPower.ApmPresent;
  cbxUPS.Checked:=APM.SystemPower.UpsPresent;
  cbxTC.Checked:=APM.SystemPower.ThermalControl;
  cbxVD.Checked:=APM.SystemPower.VideoDimPresent;
  cbxPT.Checked:=APM.SystemPower.ProcessorThrottle;
  cbxDSD.Checked:=APM.SystemPower.DiskSpinDown;
  cbxPB.Checked:=APM.SystemPower.PowerButtonPresent;
  cbxSB.Checked:=APM.SystemPower.SleepButtonPresent;
  cbxLS.Checked:=APM.SystemPower.LidPresent;
  cbxSysBat.Checked:=APM.SystemPower.SystemBatteriesPresent;

  eLWT.Text:=FormatSeconds(APM.LastWakeTime);
  eLST.Text:=FormatSeconds(APM.LastSleepTime);}
end;

end.
