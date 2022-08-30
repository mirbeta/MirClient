unit Details;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, CheckLst;

type
  TwndDetails = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    bOK: TButton;
    Panel25: TPanel;
    Label60: TLabel;
    Bevel7: TBevel;
    Label66: TLabel;
    eSMVer: TEdit;
    pcSM: TPageControl;
    tsBIOS: TTabSheet;
    GroupBox21: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label53: TLabel;
    eBIOSVendor: TEdit;
    eBIOSVer: TEdit;
    eBIOSDate: TEdit;
    eBIOSSize: TEdit;
    tsSMSystem: TTabSheet;
    GroupBox4: TGroupBox;
    Label61: TLabel;
    Label62: TLabel;
    lSysVer: TLabel;
    lSysSer: TLabel;
    lSysID: TLabel;
    eSysMod: TEdit;
    eSysMan: TEdit;
    eSysVer: TEdit;
    eSysSer: TEdit;
    eSysID: TEdit;
    tsSMMB: TTabSheet;
    GroupBox15: TGroupBox;
    Label67: TLabel;
    Label68: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    eMBMod: TEdit;
    eMBMan: TEdit;
    eMBVer: TEdit;
    eMBSer: TEdit;
    tsSMCH: TTabSheet;
    GroupBox18: TGroupBox;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    eCHMod: TEdit;
    eCHMan: TEdit;
    eCHVer: TEdit;
    eCHSer: TEdit;
    tsSMMC: TTabSheet;
    GroupBox22: TGroupBox;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label109: TLabel;
    eMCI: TEdit;
    eMCSS: TEdit;
    eMCST: TEdit;
    eMCSV: TEdit;
    eMCMS: TEdit;
    eMCSC: TEdit;
    tsSMCPU: TTabSheet;
    Panel33: TPanel;
    lvProcs: TListView;
    tsSMCaches: TTabSheet;
    Panel32: TPanel;
    lvCache: TListView;
    tsSMMem: TTabSheet;
    Panel26: TPanel;
    lvMem: TListView;
    tsSMPort: TTabSheet;
    Panel27: TPanel;
    lvPort: TListView;
    tsSMSlot: TTabSheet;
    Panel28: TPanel;
    lvSlot: TListView;
    tsSMTables: TTabSheet;
    Panel29: TPanel;
    lvTables: TListView;
    eSMTables: TEdit;
    Label1: TLabel;
    eCHAT: TEdit;
    Label2: TLabel;
    eMBAT: TEdit;
    Label7: TLabel;
    eMBLIC: TEdit;
    tsOBD: TTabSheet;
    Panel1: TPanel;
    lvOBD: TListView;
    tsTP: TTabSheet;
    Panel4: TPanel;
    lvTP: TListView;
    tsMemDev: TTabSheet;
    Panel40: TPanel;
    lvMemDev: TListView;
    Label8: TLabel;
    eBIOSSV: TEdit;
    eBIOSECFV: TEdit;
    Label9: TLabel;
    clbBIOS: TCheckListBox;
    tsCD: TTabSheet;
    Panel5: TPanel;
    lvCD: TListView;
    tsVP: TTabSheet;
    Panel6: TPanel;
    lvVP: TListView;
    tsCP: TTabSheet;
    Panel7: TPanel;
    lvCP: TListView;
    tsPMA: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    ePMALoc: TEdit;
    ePMAUse: TEdit;
    ePMAECT: TEdit;
    ePMAMC: TEdit;
    ePMADN: TEdit;
    tsOBDX: TTabSheet;
    lvOBDX: TListView;
    tsSPS: TTabSheet;
    Label13: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    eSPSLoc: TEdit;
    eSPSPUG: TEdit;
    eSPSDN: TEdit;
    eSPSMPC: TEdit;
    eSPSM: TEdit;
    Label22: TLabel;
    eSPSSN: TEdit;
    Label23: TLabel;
    eSPSATN: TEdit;
    Label24: TLabel;
    eSPSMPN: TEdit;
    Label25: TLabel;
    eSPSRL: TEdit;
    tsTPM: TTabSheet;
    lvTPM: TListView;
    tsPB: TTabSheet;
    lvPB: TListView;
    procedure FormCreate(Sender: TObject);
    procedure clbBIOSClickCheck(Sender: TObject);
  private
  public
  end;

var
  wndDetails: TwndDetails;

implementation

{$R *.DFM}

procedure TwndDetails.clbBIOSClickCheck(Sender: TObject);
begin
  clbBIOS.Checked[clbBIOS.ItemIndex]:=not clbBIOS.Checked[clbBIOS.ItemIndex];
end;

procedure TwndDetails.FormCreate(Sender: TObject);
begin
  pcSM.ActivePage:=tsBIOS;
end;

end.
