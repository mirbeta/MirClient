unit ResourcesDlg;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, MiTeC_NTDDK;

type
  Tdlg_db_Resources = class(TForm)
    Panel: TPanel;
    Panel2: TPanel;
    bOK: TButton;
    Panel3: TPanel;
    DMAList: TListView;
    Panel4: TPanel;
    IRQList: TListView;
    pMem: TPanel;
    MemList: TListView;
    Panel6: TPanel;
    PortList: TListView;
    Panel7: TPanel;
    DSDList: TListView;
    GroupBox1: TGroupBox;
    lIntfType: TLabel;
    lBusNumber: TLabel;
    lVersion: TLabel;
    lRevision: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

procedure ShowResourcesDlg(ATitle: string; ADR: TDeviceResources);

var
  dlg_db_Resources: Tdlg_db_Resources;

implementation

{$R *.lfm}

procedure ShowResourcesDlg;
var
  i: Integer;
begin
  with Tdlg_db_Resources.Create(Application.MainForm) do begin
    Caption:=ATitle;
    lIntfType.Caption:=Format('Interface Type: %s',[DeviceIntfTypeStr(ADR.InterfaceType)]);
    lBusNumber.Caption:=Format('Bus Number: %d',[ADR.BusNumber]);
    lVersion.Caption:=Format('Version: %d',[ADR.Version]);
    lRevision.Caption:=Format('Revision: %d',[ADR.Revision]);
    for i:=0 to High(ADR.Resources) do
      case ADR.Resources[i].Typ of
        CmResourceTypePort: with PortList.Items.Add do begin
          Caption:=Format('0x%8.8x',[ADR.Resources[i].Port.Start.QuadPart]);
          SubItems.Add(Format('0x%x',[ADR.Resources[i].Port.Length]));
          SubItems.Add(Format('%s',[PortTypeStr(ADR.Resources[i].Flags)]));
        end;
        CmResourceTypeInterrupt: with IRQList.Items.Add do begin
          Caption:=Format('%d',[ADR.Resources[i].Interrupt.Vector]);
          SubItems.Add(Format('%d',[ADR.Resources[i].Interrupt.Level]));
          SubItems.Add(Format('0x%x',[ADR.Resources[i].Interrupt.Affinity]));
          SubItems.Add(Format('%s',[InterruptTypeStr(ADR.Resources[i].Flags)]));
        end;
        CmResourceTypeMemory: with MemList.Items.Add do begin
          Caption:=Format('0x%8.8x',[ADR.Resources[i].Memory.Start.QuadPart]);
          SubItems.Add(Format('0x%x',[ADR.Resources[i].Memory.Length]));
          SubItems.Add(Format('%s',[MemoryAccessStr(ADR.Resources[i].Flags)]));
        end;
        CmResourceTypeDma: with DMAList.Items.Add do begin
          Caption:=Format('%d',[ADR.Resources[i].DMA.Channel]);
          SubItems.Add(Format('%d',[ADR.Resources[i].DMA.Port]));
        end;
        CmResourceTypeDeviceSpecific: with DSDList.Items.Add do begin
          Caption:=Format('0x%x',[ADR.Resources[i].DeviceSpecificData.Reserved1]);
          SubItems.Add(Format('0x%x',[ADR.Resources[i].DeviceSpecificData.Reserved2]));
          SubItems.Add(Format('0x%x',[ADR.Resources[i].DeviceSpecificData.DataSize]));
        end;
      end;
    ShowModal;
    Free;
  end;
end;

procedure Tdlg_db_Resources.FormCreate(Sender: TObject);
begin
  {Panel3.ParentBackground:=False;
  Panel4.ParentBackground:=False;
  Panel7.ParentBackground:=False;
  Panel6.ParentBackground:=False;
  pMem.ParentBackground:=False;}
end;

end.
