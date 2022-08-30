{$INCLUDE ..\..\Compilers.inc}
unit OverviewDlg;

interface

uses {$IFDEF RAD17PLUS}System.ImageList,System.UITypes,{$ENDIF}
     {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls, VCL.Buttons,
     VCL.ImgList,VCL.CheckLst, VCL.Mask, Vcl.Imaging.JPEG,
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JPEG,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls, CheckLst, Buttons, Mask,
     {$ENDIF}
     MSI_Common, MSI_SystemInfo, MSI_Storage, MSI_USB, MiTeC_CfgMgrSetupApi;

type
  TInfoPage = (pgWksta, pgOS, pgCPU, pgMem, pgDisplay, pgMonitor, pgAPM,
               pgMedia, pgNet, pgDev, pgPrn, pgStorage, pgUSB, pgEng, pgDisk, pgTZ,
               pgStartup, pgSoftware, pgProcesses, pgDrivers, pgServices, pgBluetooth,
               pgEventLog, pgAD, pgSecurity, pgWIFI);

  TInfoPages = set of TInfoPage;

const
  pgAll = [pgWksta, pgOS, pgCPU, pgMem, pgDisplay, pgMonitor, pgAPM, pgMedia,
           pgNet, pgDev, pgPrn, pgEng, pgStorage, pgUSB, pgDisk, pgTZ, pgStartup,
           pgSoftware, pgProcesses, pgServices, {pgDrivers,} pgBluetooth, pgEventLog, pgAD, pgSecurity, pgWIFI];


const
  iiComputer      = 0;
  iiSystem        = 1;
  iiDisplay       = 2;
  iiMonitor       = 3;
  iiVolumes       = 4;
  iiFDD           = 5;
  iiHDD           = 6;
  iiCDROM         = 7;
  iiTape          = 8;
  iiAPM           = 9;
  iiImaging       = 10;
  iiKeyboard      = 11;
  iiMouse         = 12;
  iiModem         = 13;
  iiPort          = 14;
  iiAdapter       = 15;
  iiPackage       = 16;
  iiSCSI          = 17;
  iiDriver        = 18;
  iiSound         = 19;
  iiUSB           = 20;
  iiGame          = 21;
  iiNet           = 22;
  iiProcess       = 23;
  iiPCMCIA        = 24;
  iiChanger       = 25;
  iiHID           = 26;
  iiGPS           = 27;
  iiReader        = 28;
  iiInfrared      = 29;
  iiMIDI          = 30;
  iiWave          = 31;
  iiMixer         = 32;
  iiAUX           = 33;
  iiDirectX       = 34;
  iiPrinter       = 35;
  iiPrinterDef    = 36;
  iiNetPrinter    = 37;
  iiNetPrinterDef = 38;
  iiController    = 39;
  iiMemory        = 40;
  iiCPU           = 41;
  iiBluetooth     = 42;
  iiMTD           = 43;
  iiSoftware      = 16;
  iiSecurity      = 44;
  iiBiometric     = 45;
  iiWPD           = 46;
  iiOther         = 47;

type
  Tdlg_msi_Overview = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    bRefresh: TButton;
    Panel3: TPanel;
    pc: TPageControl;
    tsWksta: TTabSheet;
    tsOS: TTabSheet;
    tsCPU: TTabSheet;
    pNumLock: TPanel;
    pCapsLock: TPanel;
    pScrollLock: TPanel;
    bOK: TButton;
    Image2: TImage;
    lEdition: TLabel;
    Bevel1: TBevel;
    icoCPU: TImage;
    tsMemory: TTabSheet;
    tsDisplay: TTabSheet;
    tsAPM: TTabSheet;
    tsMedia: TTabSheet;
    Label42: TLabel;
    tsNetwork: TTabSheet;
    tsDevices: TTabSheet;
    psd: TPrinterSetupDialog;
    tsPrinters: TTabSheet;
    lvPrinter: TListView;
    bPrint: TButton;
    tsEngines: TTabSheet;
    tsDrives: TTabSheet;
    Label54: TLabel;
    cbDrive: TComboBox;
    GroupBox19: TGroupBox;
    imgDrive: TImage;
    lDriveType: TLabel;
    clbFlags: TCheckListBox;
    Label55: TLabel;
    Label56: TLabel;
    eUNC: TEdit;
    eDSN: TEdit;
    Bevel3: TBevel;
    lCapacity: TLabel;
    lFree: TLabel;
    lBPS: TLabel;
    lSPC: TLabel;
    lTC: TLabel;
    lFC: TLabel;
    gDisk: TProgressBar;
    tsTZ: TTabSheet;
    Panel12: TPanel;
    Image10: TImage;
    gbStd: TGroupBox;
    Label12: TLabel;
    Label73: TLabel;
    eStdStart: TEdit;
    eStdBias: TEdit;
    eTZ: TEdit;
    Label74: TLabel;
    gbDay: TGroupBox;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    edayStart: TEdit;
    eDayBias: TEdit;
    pcCPU: TPageControl;
    tsID: TTabSheet;
    tsFeatures: TTabSheet;
    GroupBox5: TGroupBox;
    lGeneric: TLabel;
    lSerial: TLabel;
    lVendor: TLabel;
    tsStartup: TTabSheet;
    tsSoftware: TTabSheet;
    tcStartup: TTabControl;
    bNTSpec: TButton;
    lvMedia: TListView;
    lvSound: TListView;
    Label14: TLabel;
    lvStartup: TListView;
    pcOS: TPageControl;
    tsGeneral: TTabSheet;
    tsFolders: TTabSheet;
    tsInternet: TTabSheet;
    FolderList: TListView;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    eBrowser: TEdit;
    eCT: TEdit;
    email: TEdit;
    Bevel2: TBevel;
    eMachine: TEdit;
    pcEng: TPageControl;
    tsDirectX: TTabSheet;
    tsASPI: TTabSheet;
    Panel19: TPanel;
    lvDirectX: TListView;
    ilEng: TImageList;
    Panel22: TPanel;
    lvASPI: TListView;
    lASPI: TLabel;
    lDirectX: TLabel;
    tsLocale: TTabSheet;
    LocaleList: TListView;
    GroupBox7: TGroupBox;
    icoMemory: TImage;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    ePT: TEdit;
    ePF: TEdit;
    eFT: TEdit;
    eFF: TEdit;
    eVT: TEdit;
    eVF: TEdit;
    GroupBox9: TGroupBox;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    eAG: TEdit;
    eAppAddr: TEdit;
    ePS: TEdit;
    pcMachine: TPageControl;
    tsMachineGeneral: TTabSheet;
    tsMachineSMBIOS: TTabSheet;
    gbMID: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    icoComputer: TImage;
    eWksta: TEdit;
    eUser: TEdit;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    eLastBoot: TEdit;
    eSysTime: TEdit;
    Panel25: TPanel;
    Label60: TLabel;
    eSMVer: TEdit;
    Bevel7: TBevel;
    pcSM: TPageControl;
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
    tsSMCPU: TTabSheet;
    tsSMMem: TTabSheet;
    lvMem: TListView;
    tsSMPort: TTabSheet;
    lvPort: TListView;
    tsSMSlot: TTabSheet;
    lvSlot: TListView;
    tsSMTables: TTabSheet;
    lvTables: TListView;
    Label66: TLabel;
    eSMTables: TEdit;
    Panel6: TPanel;
    pcNET: TPageControl;
    tsNetGeneral: TTabSheet;
    Label47: TLabel;
    bProto: TButton;
    bServ: TButton;
    bCli: TButton;
    tsNetWinsock: TTabSheet;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    eWSDesc: TEdit;
    eWSVer: TEdit;
    eWSStat: TEdit;
    tsSMCaches: TTabSheet;
    lvCache: TListView;
    tsCache: TTabSheet;
    GroupBox20: TGroupBox;
    Panel34: TPanel;
    lMarket: TLabel;
    lTech: TLabel;
    lFreq: TLabel;
    icoNet: TImage;
    pcDisplay: TPageControl;
    tsDisplay1: TTabSheet;
    tsDisplay2: TTabSheet;
    GroupBox10: TGroupBox;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label65: TLabel;
    eChip: TEdit;
    eDAC: TEdit;
    eMem: TEdit;
    eBIOSString: TEdit;
    Panel35: TPanel;
    lvDisp: TListView;
    GroupBox11: TPanel;
    bCurve: TButton;
    bLine: TButton;
    bPoly: TButton;
    bRaster: TButton;
    bText: TButton;
    Button1: TButton;
    Button2: TButton;
    tsStorage: TTabSheet;
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
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    eSBCopy: TEdit;
    eSBName: TEdit;
    eSBDate: TEdit;
    Label100: TLabel;
    eSBExt: TEdit;
    Panel36: TPanel;
    pcDev: TPageControl;
    tsDevTree: TTabSheet;
    tsDevRes: TTabSheet;
    Tree: TTreeView;
    bProps: TButton;
    bRes: TButton;
    ResList: TListView;
    tsSMMC: TTabSheet;
    GroupBox22: TGroupBox;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    eMCI: TEdit;
    eMCSS: TEdit;
    eMCST: TEdit;
    eMCSV: TEdit;
    eMCMS: TEdit;
    Label108: TLabel;
    Label109: TLabel;
    eMCSC: TEdit;
    tsUSB: TTabSheet;
    Panel10: TPanel;
    StorageTree: TTreeView;
    tsProcesses: TTabSheet;
    ProcList: TListView;
    Label83: TLabel;
    eCHAT: TEdit;
    Label84: TLabel;
    eMBAT: TEdit;
    Label85: TLabel;
    eMBLIC: TEdit;
    lvProcs: TListView;
    tsOBD: TTabSheet;
    lvOBD: TListView;
    tsMemDev: TTabSheet;
    lvMemDev: TListView;
    tsMonitor: TTabSheet;
    cbMon: TComboBox;
    icoMonitor: TImage;
    GroupBox23: TGroupBox;
    Label52: TLabel;
    Label86: TLabel;
    Label90: TLabel;
    eMonName: TEdit;
    eMonSize: TEdit;
    eMonSN: TEdit;
    Label88: TLabel;
    eMonMan: TEdit;
    Label89: TLabel;
    eGamma: TEdit;
    Label96: TLabel;
    eMonDate: TEdit;
    Label110: TLabel;
    eEDID: TEdit;
    Label111: TLabel;
    eMonMod: TEdit;
    mProxy: TMemo;
    tsEng: TTabSheet;
    Label112: TLabel;
    eMonPN: TEdit;
    lRevision: TLabel;
    cbCPU: TComboBox;
    lCPU: TLabel;
    lCodename: TLabel;
    CacheBox: TMemo;
    tc: TTabControl;
    GroupBox6: TGroupBox;
    Panel4: TPanel;
    clbFS: TCheckListBox;
    lvStorage: TListView;
    Splitter1: TSplitter;
    tsDrivers: TTabSheet;
    tsServices: TTabSheet;
    DrvList: TListView;
    SvcList: TListView;
    TabSheet1: TTabSheet;
    UpdList: TListView;
    pcSW: TPageControl;
    tsSWApp: TTabSheet;
    tsSWMSP: TTabSheet;
    lvSW: TListView;
    lvMSP: TListView;
    TabSheet2: TTabSheet;
    EnvList: TListView;
    Panel5: TPanel;
    lvEng: TListView;
    lArch: TLabel;
    lCPP: TLabel;
    lLPC: TLabel;
    Label82: TLabel;
    Label87: TLabel;
    lvOS: TListView;
    lVersion: TLabel;
    Panel7: TPanel;
    icoMachine: TImage;
    eAC: TLabel;
    pcAPM: TPageControl;
    tsBat: TTabSheet;
    icoBattery: TImage;
    Label41: TLabel;
    Label39: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label38: TLabel;
    Label40: TLabel;
    Label63: TLabel;
    eBatLife: TEdit;
    pbBat: TProgressBar;
    eBat: TEdit;
    eBatCap: TEdit;
    eBatRemain: TEdit;
    eBatRate: TEdit;
    eBatChem: TEdit;
    eBatMan: TEdit;
    eBatVolt: TEdit;
    eBatID: TEdit;
    cbBat: TComboBox;
    tsProc: TTabSheet;
    icoProc: TImage;
    Label72: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    cbProc: TComboBox;
    eFreq: TEdit;
    eMaxFreq: TEdit;
    eFreqLimit: TEdit;
    eMaxIdle: TEdit;
    eIdle: TEdit;
    pbCPU: TProgressBar;
    TabSheet6: TTabSheet;
    Image12: TImage;
    eAPM: TEdit;
    APMPanel: TPanel;
    Label95: TLabel;
    Label113: TLabel;
    cbxUPS: TCheckBox;
    cbxTC: TCheckBox;
    cbxVD: TCheckBox;
    cbxPT: TCheckBox;
    cbxDSD: TCheckBox;
    cbxPB: TCheckBox;
    cbxSB: TCheckBox;
    cbxLS: TCheckBox;
    cbxSysBat: TCheckBox;
    eLST: TEdit;
    eLWT: TEdit;
    tsNetEnv: TTabSheet;
    pcNetEnv: TPageControl;
    tsNEConns: TTabSheet;
    lvNEConns: TListView;
    tsNEShares: TTabSheet;
    tsNEFiles: TTabSheet;
    tsNESessions: TTabSheet;
    lvNEShares: TListView;
    lvNEFiles: TListView;
    lvNESessions: TListView;
    tsBT: TTabSheet;
    lvBT: TListView;
    lJoinName: TLabel;
    eDomain: TEdit;
    Label115: TLabel;
    eSes: TEdit;
    tsEL: TTabSheet;
    tsAD: TTabSheet;
    cbEL: TComboBox;
    lvEL: TListView;
    mEL: TMemo;
    ilEvents: TImageList;
    tvAD: TTreeView;
    lvAD: TListView;
    ilAD: TImageList;
    Label116: TLabel;
    eGUID: TEdit;
    tsPC: TTabSheet;
    PCList: TListView;
    pAD: TPanel;
    TabSheet3: TTabSheet;
    clbOSFlags: TCheckListBox;
    Label117: TLabel;
    Label118: TLabel;
    eBIOSSV: TEdit;
    eBIOSECFV: TEdit;
    clbBIOS: TCheckListBox;
    lCC: TLabel;
    lTHC: TLabel;
    icoVideo: TImage;
    Label33: TLabel;
    cbDisplay: TComboBox;
    bModes: TButton;
    GroupBox12: TGroupBox;
    Label101: TLabel;
    Label102: TLabel;
    eBIOS: TEdit;
    edate: TEdit;
    BIOSList: TListView;
    tsVP: TTabSheet;
    tsTP: TTabSheet;
    tsCP: TTabSheet;
    tsCD: TTabSheet;
    lvVP: TListView;
    lvTP: TListView;
    lvCP: TListView;
    lvCD: TListView;
    GenuineIcon: TImage;
    tsSC: TTabSheet;
    pAV: TPanel;
    lvAV: TListView;
    pAS: TPanel;
    lvAS: TListView;
    pFW: TPanel;
    lvFW: TListView;
    tsWIFI: TTabSheet;
    ilWIFI: TImageList;
    lvWIFI: TListView;
    eLastShut: TEdit;
    Label48: TLabel;
    gbMemUsage: TGroupBox;
    Label13: TLabel;
    Label50: TLabel;
    Label49: TLabel;
    Label51: TLabel;
    gMemory: TProgressBar;
    gPhys: TProgressBar;
    gPage: TProgressBar;
    gVirt: TProgressBar;
    Bevel4: TBevel;
    pcUSB: TPageControl;
    tsUSBConnections: TTabSheet;
    tsUSBHistory: TTabSheet;
    lvUSBH: TListView;
    pcWIFI: TPageControl;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    lvWLANC: TListView;
    pcSec: TPageControl;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    lvWFW: TListView;
    Panel8: TPanel;
    cbxDomain: TCheckBox;
    cbxPublic: TCheckBox;
    cbxPrivate: TCheckBox;
    TabSheet9: TTabSheet;
    lvNetCreds: TListView;
    ilSystem: TImageList;
    ilPrinter: TImageList;
    Panel9: TPanel;
    Splitter2: TSplitter;
    lvUSB: TListView;
    USBTree: TTreeView;
    Label114: TLabel;
    eMonRes: TEdit;
    cbxMonPrim: TCheckBox;
    Label119: TLabel;
    eMonDesc: TEdit;
    tsOSWEI: TTabSheet;
    lSystemScore: TLabel;
    lCpuScore: TLabel;
    lMemoryScore: TLabel;
    lGraphicsScore: TLabel;
    lGamingScore: TLabel;
    lDiskScore: TLabel;
    Label120: TLabel;
    eWorkArea: TEdit;
    Label121: TLabel;
    eDPI: TEdit;
    tsPMA: TTabSheet;
    Label122: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label125: TLabel;
    Label126: TLabel;
    Label127: TLabel;
    ePMALoc: TEdit;
    ePMAUse: TEdit;
    ePMAECT: TEdit;
    ePMAMC: TEdit;
    ePMADN: TEdit;
    tsOBDX: TTabSheet;
    tsSPS: TTabSheet;
    Label128: TLabel;
    Label129: TLabel;
    Label130: TLabel;
    Label131: TLabel;
    Label132: TLabel;
    Label133: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    eSPSLoc: TEdit;
    eSPSPUG: TEdit;
    eSPSDN: TEdit;
    eSPSMPC: TEdit;
    eSPSM: TEdit;
    eSPSSN: TEdit;
    eSPSATN: TEdit;
    eSPSMPN: TEdit;
    eSPSRL: TEdit;
    tsTPM: TTabSheet;
    lvTPM: TListView;
    lvOBDX: TListView;
    lvIntf: TListView;
    bIntfDetail: TButton;
    tsPB: TTabSheet;
    lvPB: TListView;
    procedure TreeDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmRefresh(Sender: TObject);
    procedure cmClose(Sender: TObject);
    procedure cmCaps(Sender: TObject);
    procedure cmPrintSetup(Sender: TObject);
    procedure cbDriveChange(Sender: TObject);
    procedure clbClickCheck(Sender: TObject);
    procedure cmModes(Sender: TObject);
    procedure cmProto(Sender: TObject);
    procedure cmServ(Sender: TObject);
    procedure cmCli(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure cmProps(Sender: TObject);
    procedure tcStartupChange(Sender: TObject);
    procedure cmNTSpec(Sender: TObject);
    procedure cbxClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmRes(Sender: TObject);
    procedure USBTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure StorageTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure cbMonChange(Sender: TObject);
    procedure cbCPUChange(Sender: TObject);
    procedure tcChange(Sender: TObject);
    procedure StorageTreeChange(Sender: TObject; Node: TTreeNode);
    procedure cbBatChange(Sender: TObject);
    procedure cbProcChange(Sender: TObject);
    procedure cbELChange(Sender: TObject);
    procedure lvELSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tvADDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvADChange(Sender: TObject; Node: TTreeNode);
    procedure cbDisplayChange(Sender: TObject);
    procedure lvUSBHCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure USBTreeChange(Sender: TObject; Node: TTreeNode);
    procedure bIntfDetailClick(Sender: TObject);
    procedure lvIntfChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvIntfDblClick(Sender: TObject);
    procedure clbBIOSClickCheck(Sender: TObject);
  private
    FPages: TInfoPages;
    FSysInfo: TMiTeC_SystemInfo;
    sl: TStringList;
    spid: TSPClassImageListData;
    procedure SeTInfoPages(const Value: TInfoPages);
    procedure SetCaptionText(const Value: string);
    function FindUSBNode(AIndex: Integer): TTreeNode;

    procedure GetWkstaInfo;
    procedure GetOSInfo;
    procedure GetCPUInfo;
    procedure GetMemoryInfo;
    procedure GetDisplayInfo;
    procedure GetMonitorInfo;
    procedure GetAPMInfo;
    procedure GetMediaInfo;
    procedure GetNetInfo;
    procedure GetDeviceInfo;
    procedure GetEngInfo;
    procedure GetDriveInfo;
    procedure GetTZInfo;
    procedure GetPrintInfo;
    procedure GetStartupInfo;
    procedure GetSWInfo;
    procedure GetStorageInfo;
    procedure GetUSBInfo;
    procedure GetProcessesInfo;
    procedure GetServicesInfo;
    //procedure GetDriversInfo;
    procedure GetBTInfo;
    procedure GetELInfo;
    procedure GetADInfo;
    procedure GetSCInfo;
    procedure GetWIFIInfo;
    procedure LoadFromSIF(AComponent: TMiTeC_Component);
  public
    property DisplayedPages: TInfoPages read FPages write SeTInfoPages;
    property SysInfo: TMiTeC_SystemInfo read FSysInfo write FSysInfo;
    property CaptionText: string Write SetCaptionText;

    procedure RefreshData(ForceRefresh: Boolean);
    procedure DisplayData;
  end;

procedure DisplayOverviewDlg(ASI: TMiTeC_SystemInfo; APages: TInfoPages = pgAll);

var
  dlg_msi_Overview: Tdlg_msi_Overview;

implementation

uses {$IFDEF RAD9PLUS}WinAPI.ShellAPI, System.Math,{$ELSE}ShellAPI, Math,{$ENDIF}
  MiTeC_Routines, MSI_Devices, DetailDlg, MSI_OS, MSI_APM, MiTeC_Dialogs,
  MSI_Startup, MiTeC_Datetime, MSI_SMBIOS, MiTeC_PowrProf,
  MSI_Network, MSI_CPU, MiTeC_StrUtils, MSI_Machine, ResourcesDlg, MiTeC_NTDDK,
  MiTeC_USB, MiTeC_WinIOCTL, MSI_Display, MSI_Software, MiTeC_AdvAPI, MiTeC_Shares,
  MiTeC_NetAPI32, MiTeC_SysUtils, MiTeC_CtrlRtns, MSI_AD, MiTeC_ActiveDs_TLB, MSI_WIFI,
  MiTeC_Storage, MiTeC_SIF, MSI_FW, Codecs, MiTeC_Windows, MiTeC_Helpers, MiTeC_IpHlpAPI;

{$R *.DFM}

procedure DisplayOverviewDlg;
begin
  with Tdlg_msi_Overview.Create(Application.MainForm) do
    try
      SysInfo:=ASI;
      DisplayedPages:=APages;
      DisplayData;
      ShowModal;
    finally
      Free;
    end;
end;

{ TfrmMSI_Overview }

procedure Tdlg_msi_Overview.GetWIFIInfo;
var
  i: Integer;
  s: string;
  n: TListItem;
begin
  lvWIFI.Items.Clear;
  with SysInfo.WIFI do begin
    for i:=0 to NetworkCount-1 do begin
      s:=Networks[i].SSID;
      if (s='') then
        s:=Networks[i].Profile;
      n:=lvWIFI.Items.Add;
      n.Caption:=s;
      n.SubItems.Add(Format('%d%%',[Networks[i].SignalQuality]));
      n.SubItems.Add(AuthToStr(Networks[i].AuthAlgorithm));
      n.SubItems.Add(CipherToStr(Networks[i].CipherAlgorithm));
      n.SubItems.Add(PHYToStr(Networks[i].PHYType));
      n.SubItems.Add(BSSToStr(Networks[i].BSSType));
      n.SubItems.Add(Networks[i].MACAddress);
      n.SubItems.Add(Format('%d dBm',[Networks[i].RSSI]));
      n.SubItems.Add(Format('%1.3f GHz',[Networks[i].ChannelFreq/1000000]));
      n.SubItems.Add(Format('%d',[GetChannelNumber(Networks[i].ChannelFreq)]));
      n.SubItems.Add(Format('%d Mbps',[Networks[i].MaxSpeed]));
      n.SubItems.Add(Networks[i].Intf.Name);
      n.ImageIndex:=Integer(Networks[i].SecurityEnabled);
      if Networks[i].Connected then
        n.ImageIndex:=n.ImageIndex+2;
    end;
  end;

  with SysInfo.WiFiKnownNetworks do begin
    lvWLANC.Items.Clear;
    for i:=0 to RecordCount-1 do
      with lvWLANC.Items.Add do begin
        Caption:=Records[i].SSID;
        SubItems.Add(Records[i].Key);
        SubItems.Add(Records[i].Authentication);
        SubItems.Add(Records[i].Encryption);
        SubItems.Add(Records[i].Connection);
        SubItems.Add(Records[i].AdapterName);
        SubItems.Add(Records[i].IPAddress);
        SubItems.Add(DateTimeToStr(UTCToLocalDatetime(Records[i].Timestamp)));
      end;
  end;
end;

procedure Tdlg_msi_Overview.GetWkstaInfo;
var
  i: Integer;
begin
  with SysInfo.Machine do begin
    tssmTables.TabVisible:=Livedata;
    if Trim(Computer)<>'' then
      eMachine.Text:=' '+Computer+' ';
    eWksta.text:=MachineName;
    eUser.text:=User;
    if IsInDomain then
      lJoinName.Caption:='Domain'
    else
      lJoinName.Caption:='Workgroup';
    eDomain.text:=JoinedTo;
    eSBCopy.Text:=BIOS.Copyright;
    eSBName.Text:=BIOS.NameString;
    eSBDate.Text:=BIOS.Date;
    eSBExt.Text:=BIOS.ExtendedInfo;

    BIOSList.Items.Clear;
    for i:=0 to BIOS.BIOSDataCount-1 do
      with BIOSList.Items.Add do begin
        Caption:=BIOS.BIOSData[i].Name;
        SubItems.Add(BIOS.BIOSData[i].Value);
      end;

    with BIOSList.Items.Add do begin
      Caption:='UEFI';
      SubItems.Add(BooleanEn[SMBIOS.UEFI]);
    end;

    eLastBoot.text:=datetimetostr(LastBoot);
    eLastShut.text:=datetimetostrDef(LastShutdown);
    eSysTime.text:=FormatSeconds(SystemUpTime);
    eSes.Text:=GetSessionStr(Session);
    if ((stTerminal in Session) or (stCitrix in Session)) and (SessionProtocol<>spNone) then
      eSes.Text:=eSes.Text+' - '+cSessionProto[SessionProtocol];
    if NumLock then
      pNumLock.color:=clLime
    else
      pNumLock.color:=clSilver;
    if CapsLock then
      pCapsLock.color:=clLime
    else
      pCapsLock.color:=clSilver;
    if ScrollLock then
      pScrollLock.color:=clLime
    else
      pScrollLock.color:=clSilver;

    with SMBIOS do begin
      eSMVer.Text:=Format('%d.%d Rev. %d.%d',[MajorVersion,MinorVersion,RevisionMajor,RevisionMinor]);
      eSMTables.Text:=Format('%d',[Length(StructTables)]);

      if BIOSVendor='' then
        eBIOSVendor.Text:=BIOS.Copyright
      else
        eBIOSVendor.Text:=BIOSVendor;
      if BIOSVersion='' then
        eBIOSVer.Text:=BIOS.Name+' '+BIOS.ExtendedInfo
      else
        eBIOSVer.Text:=BIOSVersion;
      if BIOSDate='' then
        eBIOSDate.Text:=BIOS.Date
      else
        eBIOSDate.Text:=BIOSDate;
      eBIOSSize.Text:=Format('%d',[BIOSSize]);

      eBIOSSV.Text:=Format('%d.%d',[BIOSMajorVersion,BIOSMinorVersion]);
      eBIOSECFV.Text:=Format('%d.%d',[BIOS_ECF_MajorVersion,BIOS_ECF_MinorVersion]);

      for i:=0 to clbBIOS.Items.Count-1 do
        clbBIOS.Checked[i]:=TSMBIOS_BIOSChar(i) in BIOSCharacteristics;

      eSysMod.Text:=SystemModel;
      eSysMan.Text:=SystemManufacturer;
      eSysVer.Text:=SystemVersion;
      eSysSer.Text:=SystemSerial;
      eSysID.Text:=SystemUUID;

      eMBMod.Text:=MainBoardModel;
      eMBMan.Text:=MainBoardManufacturer;
      eMBVer.Text:=MainBoardVersion;
      eMBSer.Text:=MainBoardSerial;
      eMBAT.Text:=MainBoardAssetTag;
      eMBLIC.Text:=MainBoardLocationInChassis;

      try
        eCHMod.Text:=ChassisTypes[ChassisModel];
      except
        eCHMod.Text:=Format('unknown(%d)',[Integer(ChassisModel)]);
      end;
      eCHMan.Text:=ChassisManufacturer;
      eCHVer.Text:=ChassisVersion;
      eCHSer.Text:=ChassisSerial;
      eCHAT.Text:=ChassisAssetTag;

      try
        eMCI.Text:=InterleaveSupports[MemCtrlCurrentInterleave];
      except
        eMCI.Text:=Format('unknown(%d)',[Integer(MemCtrlCurrentInterleave)]);
      end;
      try
        eMCI.Text:=eMCI.Text+' / '+InterleaveSupports[MemCtrlSupportedInterleave];
      except
        eMCI.Text:=eMCI.Text+' / '+Format('unknown(%d)',[Integer(MemCtrlSupportedInterleave)]);
      end;
      eMCSS.Text:=GetMemorySpeedStr(MemCtrlSupportedSpeeds);
      eMCST.Text:=GetMemoryTypeStr(MemCtrlSupportedTypes);
      eMCSV.Text:=GetMemoryVoltageStr(MemCtrlSupportedVoltages);
      eMCMS.Text:=Format('%d',[MemCtrlMaxSize]);
      eMCSC.Text:=Format('%d',[MemCtrlSlotCount]);

      lvProcs.Items.Clear;
      for i:=0 to ProcessorCount-1 do
        with lvProcs.Items.Add do begin
          Caption:=Processor[i].Manufacturer;
          SubItems.Add(Processor[i].Version);
          SubItems.Add(Processor[i].Socket);
          try
            SubItems.Add(Upgrades[Processor[i].Upgrade]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Processor[i].Upgrade)]));
          end;
          SubItems.Add(Format('%1.1f V',[Processor[i].Voltage]));
          SubItems.Add(Format('%d MHz',[Processor[i].Frequency]));
          SubItems.Add(Format('%d MHz',[Processor[i].ExternalClock]));
          SubItems.Add(Processor[i].SerialNumber);
          SubItems.Add(Processor[i].AssetTag);
          SubItems.Add(Processor[i].PartNumber);
          SubItems.Add(IntToStr(Processor[i].CoreCount));
          SubItems.Add(IntToStr(Processor[i].ThreadCount));
          ImageIndex:=-1;
        end;

      lvCache.Items.Clear;
      for i:=0 to CacheCount-1 do
        with lvCache.Items.Add do begin
          Caption:=Cache[i].Designation;
          try
            SubItems.Add(CacheTypes[Cache[i].Typ]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Cache[i].Typ)]));
          end;
          try
            SubItems.Add(CacheAssociativities[Cache[i].Associativity]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Cache[i].Associativity)]));
          end;
          try
            SubItems.Add(SRAMTypes[Cache[i].SRAMType]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Cache[i].SRAMType)]));
          end;
          SubItems.Add(Format('%d KB',[Cache[i].InstalledSize]));
          SubItems.Add(Format('%d KB',[Cache[i].MaxSize]));
          SubItems.Add(Format('%d ns',[Cache[i].Speed]));
          ImageIndex:=-1;
        end;

      lvMem.Items.Clear;
      for i:=0 to MemoryModuleCount-1 do
        with lvMem.Items.Add do begin
          Caption:=MemoryModule[i].Socket;
          SubItems.Add(GetMemoryTypeStr(MemoryModule[i].Types));
          SubItems.Add(Format('%d MB',[MemoryModule[i].Size]));
          SubItems.Add(Format('%d ns',[MemoryModule[i].Speed]));
          ImageIndex:=-1;
        end;

      lvMemDev.Items.Clear;
      for i:=0 to MemoryDeviceCount-1 do
        with lvMemDev.Items.Add do begin
          Caption:=MemoryDevice[i].DeviceLocator;
          SubItems.Add(MemoryDevice[i].BankLocator);
          try
            SubItems.Add(MemoryDeviceTypes[MemoryDevice[i].Device]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Memorydevice[i].Device)]));
          end;
          SubItems.Add(GetMemoryTypeDetailsStr(MemoryDevice[i].TypeDetails));
          try
            SubItems.Add(MemoryFormFactors[MemoryDevice[i].FormFactor]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(MemoryDevice[i].FormFactor)]));
          end;
          SubItems.Add(Format('%d MB',[MemoryDevice[i].Size]));
          SubItems.Add(Format('%d MHz',[MemoryDevice[i].Speed]));
          SubItems.Add(Format('%d b',[MemoryDevice[i].TotalWidth]));
          SubItems.Add(Format('%d b',[MemoryDevice[i].DataWidth]));
          SubItems.Add(MemoryDevice[i].Manufacturer);
          SubItems.Add(MemoryDevice[i].SerialNumber);
          SubItems.Add(MemoryDevice[i].AssetTag);
          SubItems.Add(MemoryDevice[i].PartNumber);
          try
            SubItems.Add(MemoryTechnologies[MemoryDevice[i].MemoryTechnology]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(MemoryDevice[i].MemoryTechnology)]));
          end;
          ImageIndex:=-1;
        end;

      lvPort.Items.Clear;
      for i:=0 to PortCount-1 do
        with lvPort.Items.Add do begin
          Caption:=PortTypes[Port[i].Typ];
          SubItems.Add(Port[i].InternalDesignator);
          try
            SubItems.Add(ConnectorTypes[Port[i].InternalConnector]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Port[i].InternalConnector)]));
          end;
          SubItems.Add(Port[i].ExternalDesignator);
          try
            SubItems.Add(ConnectorTypes[Port[i].ExternalConnector]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Port[i].ExternalConnector)]));
          end;
          ImageIndex:=-1;
        end;

      lvSlot.Items.Clear;
      for i:=0 to SystemSlotCount-1 do
        with lvSlot.Items.Add do begin
          try
            Caption:=SlotTypes[SystemSlot[i].Typ];
          except
            Caption:=Format('unknown(%d)',[Integer(SystemSlot[i].Typ)]);
          end;
          try
            SubItems.Add(DataBusTypes[SystemSlot[i].DataBus]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(SystemSlot[i].DataBus)]));
          end;
          try
            SubItems.Add(SlotUsages[SystemSlot[i].Usage]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(SystemSlot[i].Usage)]));
          end;
          try
            SubItems.Add(SlotLengths[SystemSlot[i].Length]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(SystemSlot[i].Length)]));
          end;
          SubItems.Add(IntToStr(SystemSlot[i].BusNumber));
          SubItems.Add(IntToStr(SystemSlot[i].DevNumber));
          SubItems.Add(IntToStr(SystemSlot[i].FuncNumber));
          ImageIndex:=-1;
        end;

      lvOBD.Items.Clear;
      for i:=0 to OnBoardDeviceCount-1 do
        with lvOBD.Items.Add do begin
          Caption:=OnBoardDevice[i].DeviceName;
          try
            SubItems.Add(OnBoardDeviceTypes[OnBoardDevice[i].Typ]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(OnBoardDevice[i].Typ)]));
          end;
          if OnBoardDevice[i].Status then
            SubItems.Add('Enabled')
          else
            SubItems.Add('Disabled');
          ImageIndex:=-1;
        end;

      lvOBDX.Items.Clear;
      for i:=0 to OnBoardDeviceExCount-1 do
        with lvOBDX.Items.Add do begin
          Caption:=OnBoardDeviceEx[i].DeviceName;
          SubItems.Add(OnBoardDeviceTypes[OnBoardDeviceEx[i].Typ]);
          if OnBoardDeviceEx[i].Status then
            SubItems.Add('Enabled')
          else
            SubItems.Add('Disabled');
          SubItems.Add(IntToStr(OnBoardDeviceEx[i].Instance));
          SubItems.Add(IntToStr(OnBoardDeviceEx[i].SegmentGroupNumber));
          SubItems.Add(IntToStr(OnBoardDeviceEx[i].BusNumber));
          SubItems.Add(IntToStr(OnBoardDeviceEx[i].DeviceNumber));
          SubItems.Add(IntToStr(OnBoardDeviceEx[i].FunctionNumber));
          ImageIndex:=-1;
        end;

      lvTP.Items.Clear;
      for i:=0 to TemperatureProbeCount-1 do
        with lvTP.Items.Add do begin
          Caption:=TemperatureProbe[i].Description;
          if TemperatureProbe[i].Location in [Low(TSMBIOS_TempProbeLocationType)..High(TSMBIOS_TempProbeLocationType)] then
            SubItems.Add(TempProbeLocationTypes[TemperatureProbe[i].Location])
          else
            SubItems.Add('?');
          try
            SubItems.Add(StatusTypes[TemperatureProbe[i].Status]);
          except
            SubItems.Add('?');
          end;
          if TemperatureProbe[i].NominalValue=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.1f',[TemperatureProbe[i].NominalValue/10]));
          if TemperatureProbe[i].Min=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.1f - %1.1f',[TemperatureProbe[i].Min/10,TemperatureProbe[i].Max/10]));
          if TemperatureProbe[i].Resolution=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.3f',[TemperatureProbe[i].Resolution/1000]));
          if TemperatureProbe[i].Tolerance=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format(#177'%1.1f',[TemperatureProbe[i].Tolerance/10]));
          if TemperatureProbe[i].Accuracy=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format(#177'%1.2f',[TemperatureProbe[i].Accuracy/100]));
          ImageIndex:=-1;
        end;

      lvCD.Items.Clear;
      for i:=0 to CoolingDeviceCount-1 do
        with lvCD.Items.Add do begin
          Caption:=CoolingDevice[i].Description;
          if CoolingDevice[i].Typ in [Low(TSMBIOS_CoolingType)..High(TSMBIOS_CoolingType)] then
            SubItems.Add(CoolingTypes[CoolingDevice[i].Typ])
          else
            SubItems.Add('?');
          try
            SubItems.Add(StatusTypes[CoolingDevice[i].Status]);
          except
            SubItems.Add('?');
          end;
          if CoolingDevice[i].NominalSpeed=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%d rpm',[CoolingDevice[i].NominalSpeed]));
          SubItems.Add(Format('%d',[CoolingDevice[i].GroupUnit]));
          ImageIndex:=-1;
        end;

      lvCP.Items.Clear;
      for i:=0 to CurrentProbeCount-1 do
        with lvCP.Items.Add do begin
          Caption:=CurrentProbe[i].Description;
          if CurrentProbe[i].Location in [Low(TSMBIOS_CurrProbeLocationType)..High(TSMBIOS_CurrProbeLocationType)] then
            SubItems.Add(CurrProbeLocationTypes[CurrentProbe[i].Location])
          else
            SubItems.Add('?');
          try
            SubItems.Add(StatusTypes[CurrentProbe[i].Status]);
          except
            SubItems.Add('?');
          end;
          if CurrentProbe[i].NominalValue=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.1f',[CurrentProbe[i].NominalValue/10]));
          if CurrentProbe[i].Min=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.1f - %1.1f',[CurrentProbe[i].Min/10,CurrentProbe[i].Max/10]));
          if CurrentProbe[i].Resolution=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.3f',[CurrentProbe[i].Resolution/1000]));
          if CurrentProbe[i].Tolerance=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format(#177'%1.1f',[CurrentProbe[i].Tolerance/10]));
          if CurrentProbe[i].Accuracy=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format(#177'%1.2f',[CurrentProbe[i].Accuracy/100]));
          ImageIndex:=-1;
        end;

      lvVP.Items.Clear;
      for i:=0 to VoltageProbeCount-1 do
        with lvVP.Items.Add do begin
          Caption:=VoltageProbe[i].Description;
          if VoltageProbe[i].Location in [Low(TSMBIOS_VoltProbeLocationType)..High(TSMBIOS_VoltProbeLocationType)] then
            SubItems.Add(VoltProbeLocationTypes[VoltageProbe[i].Location])
          else
            SubItems.Add('?');
          try
            SubItems.Add(StatusTypes[VoltageProbe[i].Status]);
          except
            SubItems.Add('?');
          end;
          if VoltageProbe[i].NominalValue=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.1f',[VoltageProbe[i].NominalValue/10]));
          if VoltageProbe[i].Min=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.1f - %1.1f',[VoltageProbe[i].Min/10,VoltageProbe[i].Max/10]));
          if VoltageProbe[i].Resolution=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format('%1.3f',[VoltageProbe[i].Resolution/1000]));
          if VoltageProbe[i].Tolerance=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format(#177'%1.1f',[VoltageProbe[i].Tolerance/10]));
          if VoltageProbe[i].Accuracy=$8000 then
            SubItems.Add('n/a')
          else
            SubItems.Add(Format(#177'%1.2f',[VoltageProbe[i].Accuracy/100]));
          ImageIndex:=-1;
        end;

      ePMALoc.Text:=PMALocations[PMALocation];
      ePMAUse.Text:=PMAUses[PMAUse];
      ePMAECT.Text:=PMAErrorCorrectionTypes[PMAErrorCorrectionType];
      ePMAMC.Text:=IntToStr(PMAMaximumCapacity);
      ePMADN.Text:=IntToStr(PMANumberOfMemoryDevices);

      eSPSPUG.Text:=IntToStr(SystemPowerSupplyPowerUnitGroup);
      eSPSLoc.Text:=SystemPowerSupplyLocation;
      eSPSDN.Text:=SystemPowerSupplyDeviceName;
      eSPSM.Text:=SystemPowerSupplyManufacturer;
      eSPSSN.Text:=SystemPowerSupplySerialNumber;
      eSPSATN.Text:=SystemPowerSupplyAssetTagNumber;
      eSPSMPN.Text:=SystemPowerSupplyModelPartNumber;
      eSPSRL.Text:=SystemPowerSupplyRevisionLevel;
      if SystemPowerSupplyMaxPowerCapacity=$8000 then
        eSPSMPC.Text:='n/a'
      else
        eSPSMPC.Text:=IntToStr(SystemPowerSupplyMaxPowerCapacity);

      lvPB.Items.Clear;
      for i:=0 to BatteryCount-1 do
        with lvPB.Items.Add do begin
          Caption:=Battery[i].Location;
          SubItems.Add(Battery[i].Manufacturer);
          if Battery[i].ManufacturerDate='' then
            SubItems.Add(DateToStr(EncodeDate(1980+(Battery[i].SBDSManufactureDate shr 9),(Battery[i].SBDSManufactureDate shr 5) and 15,Battery[i].SBDSManufactureDate and 31)))
          else
            SubItems.Add(Battery[i].ManufacturerDate);
          if Battery[i].SerialNumber='' then
            SubItems.Add(IntToHex(Battery[i].SBDSSerialNumber,4))
          else
            SubItems.Add(Battery[i].SerialNumber);
          SubItems.Add(Battery[i].DeviceName);
          if Battery[i].DeviceChemistry=dcUnknown then
            SubItems.Add(Battery[i].SBDSDeviceChemistry)
          else
            SubItems.Add(DeviceChemistries[Battery[i].DeviceChemistry]);
          SubItems.Add(Format('%d mWh',[Battery[i].DesignCapacity*Min(1,Battery[i].DesignCapacityMultiplier)]));
          SubItems.Add(Format('%d mV',[Battery[i].DesignVoltage]));
          ImageIndex:=-1;
        end;

      lvTPM.Items.Clear;
      for i:=0 to TPMDeviceCount-1 do
        with lvTPM.Items.Add do begin
          Caption:=TPMDevice[i].VendorID;
          SubItems.Add(Format('%d.%d',[TPMDevice[i].MajorSpecVersion,TPMDevice[i].MinorSpecVersion]));
          SubItems.Add(TPMDevice[i].Description);
          ImageIndex:=-1;
        end;

      lvTables.Items.Clear;
      for i:=0 to High(StructTables) do
        with lvTables.Items.Add do begin
          Caption:=Format('Type %d: %s',[StructTables[i].Indicator,StructTables[i].Name]);
          SubItems.Add(Format('%d',[StructTables[i].Length]));
          SubItems.Add(Format('%4.4x',[StructTables[i].Handle]));
          ImageIndex:=-1;
        end;
    end;
  end;
end;

procedure Tdlg_msi_Overview.LoadFromSIF(AComponent: TMiTeC_Component);
var
  sir: TStorageInfoRecord;
  rh: Boolean;
begin
  rh:=True;
  AComponent.LoadStorageInfo(AComponent.StorageFileName,@sir);
  case sir.SIFFormat of
    0: AComponent.LoadFromStorage(AComponent.StorageFileName,rh);
    1: AComponent.LoadFromStorage(AComponent.StorageFileName,rh,DecompressStream);
  end;
end;

procedure Tdlg_msi_Overview.lvELSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  mEL.Lines.Text:=Item.SubItems[Item.SubItems.Count-1];
end;

procedure Tdlg_msi_Overview.lvIntfChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  bIntfDetail.Enabled:=Assigned(lvIntf.Selected);
end;

procedure Tdlg_msi_Overview.lvIntfDblClick(Sender: TObject);
begin
  if Assigned(lvIntf.Selected) then
    bIntfDetailClick(nil);
end;

procedure Tdlg_msi_Overview.lvUSBHCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  v1,v2: Variant;
begin
  v1:=StrTodatetimeDef(Item1.SubItems[1],0);
  v2:=StrTodatetimeDef(Item2.SubItems[1],0);
  Compare:=CustomSort(v1,v2);
end;

procedure Tdlg_msi_Overview.GetOSInfo;
var
  i: integer;
begin
  with SysInfo.OS do begin
    GenuineIcon.Visible:=GenuineWindows;
    lvOS.Items.Clear;
    tsOS.Caption:=OSName;
    lEdition.caption:=OSEdition;
    lVersion.caption:=format('Version: %d.%d.%d',[MajorVersion,MinorVersion,BuildNumber]);
    eGUID.Text:=Uppercase(MachineGUID);
    with lvOS.Items.Add do begin
      Caption:='Product Name';
      SubItems.Add(ProductName);
    end;
    with lvOS.Items.Add do begin
      Caption:='Type';
      SubItems.Add(Version);
    end;
    with lvOS.Items.Add do begin
      Caption:='BuildLab';
      SubItems.Add(BuildLab);
    end;
    with lvOS.Items.Add do begin
      Caption:='OS Build';
      SubItems.Add(OSBuild);
    end;
    with lvOS.Items.Add do begin
      Caption:='ReleaseID';
      SubItems.Add(ReleaseId);
    end;
    with lvOS.Items.Add do begin
      Caption:='Service Pack';
      SubItems.Add(Format('%d.%d',[ServicePackMajorVersion,ServicePackMinorVersion]));
    end;
    with lvOS.Items.Add do begin
      Caption:='Installation type';
      SubItems.Add(InstallationType);
    end;
    with lvOS.Items.Add do begin
      Caption:='True Windows version';
      SubItems.Add(TrueWindowsVersion);
    end;
    if LiveID<>'' then
      with lvOS.Items.Add do begin
        Caption:='Windows LiveID';
        SubItems.Add(LiveID);
      end;
    with lvOS.Items.Add do begin
      Caption:='Number of Licenses';
      SubItems.Add(IntToStr(NumberOfLicensedUsers));
    end;
    with lvOS.Items.Add do begin
      Caption:='Number of Users';
      SubItems.Add(IntToStr(NumberOfUsers));
    end;
    with lvOS.Items.Add do begin
      Caption:='Product ID';
      SubItems.Add(ProductID);
    end;
    with lvOS.Items.Add do begin
      Caption:='Product key';
      if SameText(ProductKey,'BBBBB-BBBBB-BBBBB-BBBBB-BBBBB') then
        SubItems.Add('Multi Volume License')
      else
        SubItems.Add(ProductKey);
    end;
    with lvOS.Items.Add do begin
      Caption:='Install date';
      SubItems.Add(DateTimeToStr(InstallDate));
    end;
    with lvOS.Items.Add do begin
      Caption:='Registered user';
      SubItems.Add(RegisteredUser);
    end;
    with lvOS.Items.Add do begin
      Caption:='Registered organization';
      SubItems.Add(RegisteredOrg);
    end;
    if DVDRegion<>'' then
      with lvOS.Items.Add do begin
        Caption:='DVD region';
        SubItems.Add(DVDRegion);
      end;
    if (MajorVersion>=6) then begin
      with lvOS.Items.Add do begin
        Caption:='User Account Control';
        if UserAccountControl then
          SubItems.Add('Enabled')
        else
          SubItems.Add('Disabled');
      end;
      with lvOS.Items.Add do begin
        Caption:='Virtualization';
        if Virtualization then
          SubItems.Add('Enabled')
        else
          SubItems.Add('Disabled');
      end;
    end;
    with lvOS.Items.Add do begin
      Caption:='Task Manager';
      SubItems.Add(TaskManager);
    end;

    sl.CommaText:=GetServerTypeFlagString(Flags);
    for i:=0 to clbOSFlags.Items.Count-1 do
      clbOSFlags.Checked[i]:=sl.IndexOf(clbOSFlags.Items[i])>=0;

    FolderList.Items.Clear;
    for i:=0 to Folders.Count-1 do
      if ListValueFromIndex(Folders,i)<>'' then
        with FolderList.Items.Add do begin
          Caption:=Folders.Names[i];
          SubItems.Add(ListValueFromIndex(Folders,i));
        end;

    UpdList.Items.Clear;
    for i:=0 to UpdateCount-1 do
      with UpdList.Items.Add do begin
        Caption:=Updates[i].ID;
        SubItems.Add(Updates[i].InstallDate);
        SubItems.Add(Updates[i].InstalledBy);
        SubItems.Add(Updates[i].Description);
      end;

    EnvList.Items.Clear;
    for i:=0 to Environment.Count-1 do
      with EnvList.Items.Add do begin
        Caption:=SysInfo.OS.Environment.Names[i];
        SubItems.Add(Environment.Values[Environment.Names[i]]);
      end;

    LocaleList.Items.Clear;
    with LocaleList.Items.Add do begin
      Caption:='Abbreviate Country Code';
      SubItems.Add(LocaleInfo.AbbreviateCountryCode);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Abbreviate Language Code';
      SubItems.Add(LocaleInfo.AbbreviateLanguageName);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Country Code';
      SubItems.Add(LocaleInfo.CountryCode);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Currency Decimal Digits';
      SubItems.Add(LocaleInfo.CurrencyDecimalDigits);
    end;

    with LocaleList.Items.Add do begin
      Caption:='Full Country Code';
      SubItems.Add(LocaleInfo.FullCountryCode);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Full English Language Name';
      SubItems.Add(LocaleInfo.FullLanguageEnglishName);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Full Localize Language Name';
      SubItems.Add(LocaleInfo.FullLocalizeLanguage);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Intl. Monetary Symbol';
      SubItems.Add(LocaleInfo.InternationalMonetarySymbol);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Local Monetary Symbol';
      SubItems.Add(LocaleInfo.LocalMonetarySymbol);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Positive Currency Mode';
      case LocaleInfo.PositiveCurrencyMode of
        Prefix_No_Separation: SubItems.Add('Prefix_No_Separation');
        Suffix_No_Separation: SubItems.Add('Suffix_No_Separation');
        Prefix_One_Char_Separation: SubItems.Add('Prefix_One_Char_Separation');
        Suffix_One_Char_Separation: SubItems.Add('Suffix_One_Char_Separation');
      end;
    end;
    with LocaleList.Items.Add do begin
      Caption:='Negative Currency Mode';
      SubItems.Add(LocaleInfo.NegativeCurrencyMode);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Currency Decimal Separator';
      SubItems.Add(LocaleInfo.CurrencyDecimalSeparator);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Currency Thousand Separator';
      SubItems.Add(LocaleInfo.CurrencyThousandSeparator);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Decimal Separator';
      SubItems.Add(LocaleInfo.DecimalSeparator);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Decimal Thousand Separator';
      SubItems.Add(LocaleInfo.DecimalThousandSeparator);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Number Of Decimal Digits';
      SubItems.Add(LocaleInfo.NumberOfDecimalDigits);
    end;
    with LocaleList.Items.Add do begin
      Caption:='List Separator';
      SubItems.Add(LocaleInfo.ListSeparator);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Date Separator';
      SubItems.Add(LocaleInfo.DateSeparator);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Long Date Order';
      case LocaleInfo.LongDateOrder of
        MDY: SubItems.Add('M-D-Y');
        DMY: SubItems.Add('D-M-Y');
        YMD: SubItems.Add('Y-M-D');
      end;
    end;
    with LocaleList.Items.Add do begin
      Caption:='Short Date Order';
      case LocaleInfo.ShortDateOrder of
        MDY: SubItems.Add('M-D-Y');
        DMY: SubItems.Add('D-M-Y');
        YMD: SubItems.Add('Y-M-D');
      end;
    end;
    with LocaleList.Items.Add do begin
      Caption:='Long Date Format';
      SubItems.Add(LocaleInfo.LongDateFormat);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Short Date Format';
      SubItems.Add(LocaleInfo.ShortDateFormat);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Time Format';
      SubItems.Add(LocaleInfo.TimeFormat);
    end;
    with LocaleList.Items.Add do begin
      Caption:='Time Format Specifier';
      case LocaleInfo.TimeFormatSpecifier of
        H12: SubItems.Add('12H');
        H24: SubItems.Add('24H');
      end;
    end;
    with LocaleList.Items.Add do begin
      Caption:='Year Format';
      case LocaleInfo.YearFormat of
        TwoDigit: SubItems.Add('Two Digit');
        FourDigit: SubItems.Add('Four Digit');
      end;
    end;
    with LocaleList.Items.Add do begin
      Caption:='Measurement System';
      case LocaleInfo.MeasurementSystem of
        Metric: SubItems.Add('Metric');
        US: SubItems.Add('U.S.');
      end;
    end;

    tsPC.TabVisible:=(MajorVersion>=6);
    PCList.Items.Clear;
    for i:=0 to ParentalControls.Count-1 do
      with PCList.Items.Add do begin
        Caption:=ParentalControls[i];
        SubItems.Add('Enabled');
      end;

    eBrowser.Text:=Internet.DefaultBrowserName;
    eMail.Text:=Internet.DefaultMailClientname;
    mProxy.Lines.Text:=Internet.ProxyServer.text;
    eCT.Text:=Internet.GetConnTypeStr(Internet.ConnectionType);

    lSystemScore.Caption:=Format('System score: %1.1f',[WindowsExperienceIndex.SystemScore]);
    lGraphicsScore.Caption:=Format('Graphics score: %1.1f',[WindowsExperienceIndex.GraphicsScore]);
    lCpuScore.Caption:=Format('Processor score: %1.1f',[WindowsExperienceIndex.CPUScore]);
    lMemoryScore.Caption:=Format('Memory score: %1.1f',[WindowsExperienceIndex.MemoryScore]);
    lDiskScore.Caption:=Format('Primary hard drive score: %1.1f',[WindowsExperienceIndex.DiskScore]);
    lGamingScore.Caption:=Format('Gaming score: %1.1f',[WindowsExperienceIndex.GamingScore]);
    tsOSWEI.TabVisible:=not SameValue(WindowsExperienceIndex.SystemScore,0);
  end;
end;

procedure Tdlg_msi_Overview.GetMemoryInfo;
begin
  with SysInfo.Memory do begin
    gbMemUsage.Visible:=LiveData;
    eAG.text:=formatfloat('#,##',AllocGranularity);
    eAppAddr.text:=format('%x - %x',[MinAppAddress,MaxAppAddress]);
    ePS.text:=formatfloat('#,##',PageSize);

    ePT.text:=formatfloat('#,##',PhysicalTotal);
    ePF.text:=formatfloat('#,#0',PhysicalFree);
    eFT.text:=formatfloat('#,##',PageFileTotal);
    eFF.text:=formatfloat('#,#0',PageFileFree);
    eVT.text:=formatfloat('#,##',VirtualTotal);
    eVF.text:=formatfloat('#,#0',VirtualFree);
    try
      gMemory.Position:=MemoryLoad;
      gPhys.Position:=Round(PhysicalFree/PhysicalTotal*100);
      gPage.Position:=Round(PageFileFree/PageFileTotal*100);
      gVirt.Position:=Round(VirtualFree/VirtualTotal*100);
    except
    end;
  end;
end;

procedure Tdlg_msi_Overview.GetDisplayInfo;
var
  i: Integer;
begin
  pcDisplay.ActivePage:=tsDisplay1;
  with SysInfo, Display do begin

    cbDisplay.Items.Clear;
    for i:=0 to AdapterCount-1 do
      cbDisplay.Items.Add(Adapter[i].Name);
    cbDisplay.ItemIndex:=0;
    cbDisplayChange(nil);

    eBIOS.text:=format('%s',[BIOSVersion]);
    edate.text:=format('%s',[BIOSDate]);
    with lvDisp, Items do begin
      Clear;
      with Add do begin
        Caption:='Technology';
        SubItems.Add(Technology);
      end;
      with Add do begin
        Caption:='Device Driver Version';
        SubItems.Add(format('%d',[DeviceDriverVersion]));
      end;
      with Add do begin
        Caption:='Resolution';
        SubItems.Add(Format('%d x %d - %d bit',[HorzRes,VertRes,ColorDepth]));
      end;
      with Add do begin
        Caption:='Physical Size';
        SubItems.Add(Format('(%d x %d) mm',[HorzSize,VertSize]));
      end;
      with Add do begin
        Caption:='Pixel Width';
        SubItems.Add(format('%d',[PixelWidth]));
      end;
      with Add do begin
        Caption:='Pixel Height';
        SubItems.Add(format('%d',[PixelHeight]));
      end;
      with Add do begin
        Caption:='Pixel Diagonal';
        SubItems.Add(format('%d',[PixelDiagonal]));
      end;
      with Add do begin
        Caption:='Font Resolution';
        SubItems.Add(format('%d dpi',[FontResolution]));
      end;
      with Add do begin
        Caption:='Vertical Refresh Rate';
        SubItems.Add(format('%d Hz',[VerticalRefreshRate]));
      end;
    end;
  end;
end;

procedure Tdlg_msi_Overview.GetADInfo;

function CreatePath(ADN: string): string;
var
  sl: TStringList;
  s: string;
  i: Integer;
begin
  Result:='';
  s:='';
  sl:=TStringList.Create;
  try
    SetDelimitedText(ADN,',',sl);
    if sl.Count<2 then
      Exit;
    for i:=sl.Count-1 downto 0 do
      if SameText(sl.Names[i],'DC') then
        s:=ListValueFromIndex(sl,i)+'.'+s
      else
        Result:=Result+IncludeTrailingPathDelimiter(StringReplace(ListValueFromIndex(sl,i),'\','/',[rfReplaceAll,rfIgnoreCase]));
    SetLength(s,Length(s)-1);
    Result:=IncludeTrailingPathDelimiter(s)+Result;
  finally
    sl.Free;
  end;
end;

var
  i: Integer;
  n: TTreeNode;
  pi: PInteger;
  s: string;
begin
  tvAD.Items.Clear;
  for i:=0 to SysInfo.ActiveDirectory.UserCount-1 do begin
    s:=CreatePath(GetDN(SysInfo.ActiveDirectory.Users[i]));
    if s<>'' then begin
      n:=Tree_CreateNodeByPath(tvAD,s,nil,1);
      n.ImageIndex:=2;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;
    end;
  end;
  for i:=0 to SysInfo.ActiveDirectory.ComputerCount-1 do begin
    s:=CreatePath(GetDN(SysInfo.ActiveDirectory.Computers[i]));
    if s<>'' then begin
      n:=Tree_CreateNodeByPath(tvAD,s,nil,1);
      n.ImageIndex:=3;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;
    end;
  end;
  for i:=0 to SysInfo.ActiveDirectory.GroupCount-1 do begin
    s:=CreatePath(GetDN(SysInfo.ActiveDirectory.Groups[i]));
    if s<>'' then begin
      n:=Tree_CreateNodeByPath(tvAD,s,nil,1);
      n.ImageIndex:=4;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;
    end;
  end;
  tvAD.Selected:=tvAD.Items.GetFirstNode;
  if not Assigned(tvAD.Selected) then
    Exit;
  tvAD.Selected.ImageIndex:=0;
  tvAD.Selected.SelectedIndex:=tvAD.Selected.ImageIndex;
  tvADChange(tvAD,tvAD.Selected);
  tvAD.Selected.AlphaSort(True);
  tvAD.Selected.Expand(False);
end;

procedure Tdlg_msi_Overview.GetAPMInfo;
var
  i: integer;
  s: string;
begin
  with SysInfo.APM do begin
    if BatteryCount=0 then
      pcAPM.ActivePage:=tsProc
    else
      pcAPM.ActivePage:=tsBat;
    tsBat.TabVisible:=BatteryCount>0;
    tsProc.TabVisible:=ProcessorCount>0;

    cbBat.Items.Clear;
    for i:=0 to BatteryCount-1 do
      cbBat.Items.Add(Format('[%d] %s',[i+1,Battery[i].Devicename]));
    cbBat.ItemIndex:=0;

    cbProc.Items.Clear;
    if ProcessorCount>0 then begin
      for i:=0 to ProcessorCount-1 do
        cbProc.Items.Add(Format('Processor #%d',[ProcessorPowerStatus[i].Number]));
      cbProc.ItemIndex:=0;
    end;

    case ACPowerStatus of
      psUnknown: s:='unknown power';
      psOnline: s:='AC power';
      psOffline: s:='Battery power';
    end;
    eAC.Caption:=Format('System is running on %s',[s]);
    cbBatChange(nil);
    if ProcessorCount>0 then
      cbProcChange(nil);
  end;
end;

procedure Tdlg_msi_Overview.GetBTInfo;
var
  i: integer;
begin
  with SysInfo, Bluetooth do begin
    lvBT.Items.BeginUpdate;
    try
      lvBT.Items.Clear;
      for i:=0 to DeviceCount-1 do
        with lvBT.Items.Add do begin
          Caption:=Devices[i].Name;
          SubItems.Add(Devices[i].Address);
          SubItems.Add(DatetimeToStrDef(devices[i].LastUsed,''));
          SubItems.Add(DatetimeToStrDef(devices[i].LastSeen,''));
          SubItems.Add(BooleanEn[Devices[i].Authenticated]);
          SubItems.Add(BooleanEn[Devices[i].Remembered]);
          SubItems.Add(BooleanEn[Devices[i].Connected]);
        end;
    finally
      lvBT.Items.EndUpdate;
    end;
  end;
end;

procedure Tdlg_msi_Overview.GetMediaInfo;
var
  i,iim,iia :integer;
  g: TGUID;
begin
  g:=GUID_DEVCLASS_MEDIA;
  SetupDiGetClassImageIndex(spid,g,iim);
  g:=GUID_DEVCLASS_MEDIA;
  SetupDiGetClassImageIndex(spid,g,iia);
  with SysInfo.Media do begin
    lvMedia.Items.beginUpdate;
    lvMedia.items.clear;
    for i:=0 to Devices.count-1 do
      with lvMedia.items.add do begin
        caption:=Devices[i];
        if SameText(Devices[i],SoundCardName) then
          imageindex:=iia
        else
          imageindex:=iim;
      end;
    lvMedia.Items.EndUpdate;
    lvSound.Items.beginUpdate;
    lvSound.items.clear;
    for i:=0 to WaveIn.count-1 do
      with lvSound.items.add do begin
        caption:=WaveIn[i];
        SubItems.Add('Wave In');
        ImageIndex:=iim;
      end;
    for i:=0 to WaveOut.count-1 do
      with lvSound.items.add do begin
        caption:=WaveOut[i];
        SubItems.Add('Wave Out');
        ImageIndex:=iim;
      end;
    for i:=0 to MIDIIn.count-1 do
      with lvSound.items.add do begin
        caption:=MIDIIn[i];
        SubItems.Add('MIDI In');
        ImageIndex:=iim;
      end;
    for i:=0 to MIDIOut.count-1 do
      with lvSound.items.add do begin
        caption:=MIDIOut[i];
        SubItems.Add('MIDI Out');
        ImageIndex:=iim;
      end;
    for i:=0 to AUX.count-1 do
      with lvSound.items.add do begin
        caption:=AUX[i];
        SubItems.Add('AUX');
        ImageIndex:=iim;
      end;
    for i:=0 to Mixer.count-1 do
      with lvSound.items.add do begin
        caption:=Mixer[i];
        SubItems.Add('Mixer');
        ImageIndex:=iim;
      end;
  end;
  lvSound.Items.endUpdate;
end;

procedure Tdlg_msi_Overview.GetNetInfo;
var
  i,iia,iic,iis :integer;
  s: string;
  g: TGUID;
begin
  g:=GUID_DEVCLASS_NET;
  SetupDiGetClassImageIndex(spid,g,iia);

  with SysInfo.Network do begin
    pcNet.ActivePage:=tsNetGeneral;
    lvIntf.items.clear;
    for i:=0 to TCPIP.AdapterCount-1 do
      with lvIntf.Items.Add do begin
        Caption:=TCPIP.Adapter[i].Name;
        SubItems.Add(TCPIP.Adapter[i].IPAddress.CommaText);
        SubItems.Add(TCPIP.Adapter[i].IPAddressMask.CommaText);
        SubItems.Add(TCPIP.Adapter[i].Address);
        SubItems.Add(AdapterTypes[TCPIP.Adapter[i].Typ]);
        SubItems.Add(GetIntfStatStr(TCPIP.Adapter[i].OperStatus));
        SubItems.Add(GetIntfAdminStr(TCPIP.Adapter[i].AdminStatus));
        SubItems.Add(IntToStr(TCPIP.Adapter[i].MaxSpeed div 1000000));
        SubItems.Add(IntToStr(TCPIP.Adapter[i].MTU));
        if TCPIP.BestInterfaceIdx=TCPIP.Adapter[i].IntfIdx then
          ImageIndex:=1
        else
          ImageIndex:=-1;
      end;
    eWSDesc.Text:=Winsock.Description;
    eWSVer.Text:=Format('%d.%d',[Winsock.MajorVersion,Winsock.MinorVersion]);
    eWSStat.Text:=Winsock.Status;
    
    with lvNEConns, Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to Resources.ConnectionCount-1 do
          with Add do begin
            Caption:=Resources.Connections[i].Name;
            SubItems.Add(Resources.Connections[i].Username);
            Subitems.Add(ShareTypes[Resources.Connections[i].ConnType]);
            SubItems.Add(IntToStr(Resources.Connections[i].OpenFiles));
            SubItems.Add(IntToStr(Resources.Connections[i].Users));
          end;
      finally
        EndUpdate;
      end;
    end;
    with lvNEShares, Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to Resources.ShareCount-1 do
          with Add do begin
            Caption:=Resources.Shares[i].Name;
            SubItems.Add(Resources.Shares[i].Path);
            Subitems.Add(ShareTypes[Resources.Shares[i].ShareType]);
          end;
      finally
        EndUpdate;
      end;
    end;
    with lvNEFiles, Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to Resources.OpenFileCount-1 do
          with Add do begin
            Caption:=Resources.OpenFiles[i].Name;
            SubItems.Add(Resources.OpenFiles[i].Username);
            s:='';
            if Resources.OpenFiles[i].Mode and ACCESS_READ<>0 then
              s:=s+'read ';
            if Resources.OpenFiles[i].Mode and ACCESS_WRITE<>0 then
              s:=s+'write ';
            if Resources.OpenFiles[i].Mode and ACCESS_CREATE<>0 then
              s:=s+'create ';
            SubItems.Add(s);
          end;
      finally
        EndUpdate;
      end;
    end;
    with lvNESessions, Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to Resources.SessionCount-1 do
          with Add do begin
            Caption:=Resources.Sessions[i].Name;
            SubItems.Add(Resources.Sessions[i].Username);
            SubItems.Add(IntToStr(Resources.Sessions[i].OpenFiles));
            SubItems.Add(Resources.Sessions[i].SesiType);
            SubItems.Add(Resources.Sessions[i].Transport);
          end;
      finally
        EndUpdate;
      end;
    end;
  end;

  with SysInfo.NetworkCredentials do begin
    lvNetCreds.Items.Clear;
    for i:=0 to RecordCount-1 do
      with lvNetCreds.Items.Add do begin
        case Records[i].Typ of
          1: Caption:='Generic';
          2: Caption:='Domain Password';
          3: Caption:='Domain Certificate';
          4: Caption:='Domain Visible Password';
          5: Caption:='Generic Certificate';
          6: Caption:='Domain Extended';
          7: Caption:='Maximum';
          1007: Caption:='Maximum Extended';
          else Caption:=IntToStr(Records[i].Typ);
        end;
        SubItems.Add(DateTimeToStr(Records[i].Timestamp));
        SubItems.Add(Records[i].Target);
        SubItems.Add(Records[i].Username);
        SubItems.Add(Records[i].Password);
      end;
  end;
end;

procedure Tdlg_msi_Overview.GetDeviceInfo;
var
  i,c,ii: integer;
  r,n: TTreeNode;
  cn,dn: string;
  pi: PInteger;
  ldc: string;
  RL: TResourceList;
  g: TGUID;
begin
  ldc:='';
  with SysInfo, Devices, Tree,Items do begin
    c:=DeviceCount-1;
    BeginUpdate;
    while Count>0 do begin
      if Assigned(Items[Count-1].Data) then
        FreeMem(Items[Count-1].Data);
      Delete(Items[Count-1]);
    end;
    r:=Add(nil,GetMachine);
    r.ImageIndex:=0;
    r.SelectedIndex:=r.ImageIndex;
    n:=nil;
    for i:=0 to c do begin
      if (Trim(Devices[i].ClassDesc)<>'') then
        cn:=Devices[i].ClassDesc
      else
        cn:=Devices[i].ClassName;
      if not Assigned(n) or not SameText(Devices[i].ClassName,ldc) then begin
        ldc:=Devices[i].ClassName;
        n:=AddChild(r,cn);
        g:=Devices[i].ClassGUID;
        SetupDiGetClassImageIndex(spid,g,ii);
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
      end;
      dn:=Devices[i].Name;
      with AddChild(n,dn) do begin
        ImageIndex:=n.ImageIndex;
        SelectedIndex:=ImageIndex;
        new(pi);
        pi^:=i;
        Data:=pi;
      end;
      n.AlphaSort;
    end;
    r.AlphaSort;
    r.Expand(False);
    EndUpdate;
  end;


  tsDevRes.TabVisible:=SysInfo.Devices.LiveData;
  bRes.Visible:=SysInfo.Devices.LiveData;
  if SysInfo.Devices.LiveData then
  with SysInfo, Devices, ResList, Items do begin
    GetResourceList(RL);
    BeginUpdate;
    try
      Clear;
      for i:=0 to High(RL) do
        with Add do begin
          Caption:=RL[i].Resource;
          SubItems.Add(ResourceShareStr(RL[i].Share));
          SubItems.Add(RL[i].Device);
          g:=RL[i].DeviceClassGUID;
          SetupDiGetClassImageIndex(spid,g,ii);
          ImageIndex:=ii;
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tdlg_msi_Overview.GetELInfo;
var
  i: Integer;
begin
  for i:=0 to SysInfo.EventLog.ContainerCount-1 do
    cbEL.Items.Add(SysInfo.EventLog.Containers[i].Name);
end;

procedure Tdlg_msi_Overview.GetEngInfo;
var
  i: integer;
begin
  lvEng.Items.Clear;
  with SysInfo.Engines do begin
    if ODBC<>'' then
      with lvEng.Items.Add do begin
        Caption:='Open Database Connectivity';
        SubItems.Add(ODBC);
        ImageIndex:=0;
      end;
    if BDE<>'' then
      with lvEng.Items.Add do begin
        Caption:='Borland Database Engine';
        SubItems.Add(BDE);
        ImageIndex:=0;
      end;
    if DAO<>'' then
      with lvEng.Items.Add do begin
        Caption:='Microsoft Data Access Objects';
        SubItems.Add(DAO);
        ImageIndex:=0;
      end;
    if ADO<>'' then
      with lvEng.Items.Add do begin
        Caption:='Microsoft ActiveX Data Objects';
        SubItems.Add(ADO);
        ImageIndex:=0;
      end;
    if OpenGL<>'' then
      with lvEng.Items.Add do begin
        Caption:='OpenGL';
        SubItems.Add(OpenGL);
        ImageIndex:=0;
      end;
    if IE<>'' then
      with lvEng.Items.Add do begin
        Caption:='Internet Explorer';
        SubItems.Add(IE);
        ImageIndex:=0;
      end;
    if NET<>'' then
      with lvEng.Items.Add do begin
        Caption:='Microsoft .NET Framework';
        SubItems.Add(NET);
        ImageIndex:=0;
      end;
    if MSI<>'' then
      with lvEng.Items.Add do begin
        Caption:='Microsoft Windows Installer';
        SubItems.Add(MSI);
        ImageIndex:=0;
      end;
    if QT<>'' then
      with lvEng.Items.Add do begin
        Caption:='Apple QuickTime';
        SubItems.Add(QT);
        ImageIndex:=0;
      end;
  end;
  with SysInfo.Engines.DirectX do begin
    if Version<>'' then begin
      lDirectX.caption:='Installed version: '+Version;
      lvDirectX.Items.beginUpdate;
      lvDirectX.items.clear;
      for i:=0 to Direct3D.count-1 do
        with lvDirectX.items.add do begin
          caption:=Direct3D[i];
          SubItems.Add('Direct3D');
          ImageIndex:=1;
        end;
      for i:=0 to DirectMusic.count-1 do
        with lvDirectX.items.add do begin
          caption:=DirectMusic[i];
          SubItems.Add('DirectMusic');
          ImageIndex:=1;
        end;
      for i:=0 to DirectPlay.count-1 do
        with lvDirectX.items.add do begin
          caption:=DirectPlay[i];
          SubItems.Add('DirectPlay');
          ImageIndex:=1;
        end;
      lvDirectX.Items.endUpdate;
    end else
      lDirectX.caption:='Not installed.';
  end;
  with SysInfo.Engines.ASPI32 do begin
    if ASPI<>'' then
      lASPI.caption:='Adaptec ASPI '+ASPI
    else
      lASPI.caption:='Adaptec ASPI not found';
    //lvASPI.Items.beginUpdate;
    lvASPI.items.Clear;
    for i:=0 to Configuration.LUN.Count-1 do
      with lvASPI.items.add do begin
        caption:=Configuration.Vendor[i]+Configuration.Model[i];
        try SubItems.Add(Configuration.Revision[i]) except SubItems.Add('') end;
        try SubItems.Add(GetTypeStr(StrToInt(Configuration.Typ[i]))) except SubItems.Add('') end;
        try SubItems.Add(Configuration.Host[i]) except SubItems.Add('') end;
        try SubItems.Add(Configuration.ID[i]) except SubItems.Add('') end;
        try SubItems.Add(Configuration.LUN[i]) except SubItems.Add('') end;
        try SubItems.Add(Configuration.Extra[i]) except SubItems.Add('') end;
        ImageIndex:=2;
      end;
    //lvASPI.Items.beginUpdate;
  end;
end;

procedure Tdlg_msi_Overview.GetDriveInfo;
var
  i,j :integer;
  s :string;
begin
  j:=0;
  with SysInfo.Disk do begin
    cbDrive.items.clear;
    for i:=1 to length(AvailableDisks) do begin
      s:=uppercase(copy(AvailableDisks,i,1));
      cbDrive.items.add(s+':');
      if s=uppercase(copy(SysInfo.OS.Folders.Values['Windows'],1,1)) then
        j:=i-1;
    end;
    cbDrive.itemindex:=j;
    cbDriveChange(nil);
  end;
end;

{procedure Tdlg_msi_Overview.GetDriversInfo;
var
  i: Integer;
begin
  with SysInfo.ProcessList do begin
    try
      DrvList.Items.BeginUpdate;
      DrvList.Items.Clear;
      for i:=0 to DriverCount-1 do
        with DrvList.Items.Add do begin
          Caption:=ExtractFilename(Drivers[i].Name);
          SubItems.Add(Format('%d',[Drivers[i].LoadCount]));
          SubItems.Add(Drivers[i].Name);
        end;
    finally
      DrvList.Items.EndUpdate;
    end;
  end;
  tsDrivers.Caption:=Format('Drivers (%d)',[SysInfo.ProcessList.DriverCount]);
end;}

procedure Tdlg_msi_Overview.DisplayData;
begin
  screen.cursor:=crhourglass;
  try
    with SysInfo do begin
      //tsDrivers.TabVisible:=tsDevRes.TabVisible;
      tsServices.TabVisible:=tsDevRes.TabVisible;
      tsAD.TabVisible:=Assigned(SysInfo.ActiveDirectory) and SysInfo.ActiveDirectory.DataAvailable;
      tsEL.TabVisible:=Assigned(SysInfo.EventLog) and SysInfo.EventLog.DataAvailable;
    end;
    if pgWksta in DisplayedPages then
      try
        GetWkstaInfo;
      except
        {$IFDEF DEBUG}
        Error('GetWkstaInfo');
        {$ENDIF}
      end;
    if pgOS in DisplayedPages then
      try
        GetOSInfo;
      except
        {$IFDEF DEBUG}
        Error('GetOSInfo');
        {$ENDIF}
      end;
    if pgCPU in DisplayedPages then
      try
        GetCPUInfo;
      except
        {$IFDEF DEBUG}
        Error('GetCPUInfo');
        {$ENDIF}
      end;
    if pgMem in DisplayedPages then
      try
        GetMemoryInfo;
      except
        {$IFDEF DEBUG}
        Error('GetMemoryInfo');
        {$ENDIF}
      end;
    if pgDisplay in DisplayedPages then
      try
        GetDisplayInfo;
      except
        {$IFDEF DEBUG}
        Error('GetDisplayInfo');
        {$ENDIF}
      end;
    if pgMonitor in DisplayedPages then
      try
        GetMonitorInfo;
      except
        {$IFDEF DEBUG}
        Error('GetMonitorInfo');
        {$ENDIF}
      end;
    if pgAPM in DisplayedPages then
      try
        GetAPMInfo;
      except
        {$IFDEF DEBUG}
        Error('GetAPMInfo');
        {$ENDIF}
      end;
    if pgMedia in DisplayedPages then
      try
        GetMediaInfo;
      except
        {$IFDEF DEBUG}
        Error('GetMediaInfo');
        {$ENDIF}
      end;
    if pgNet in DisplayedPages then
      try
        GetNetInfo;
      except
        {$IFDEF DEBUG}
        Error('GetNetInfo');
        {$ENDIF}
      end;
    if pgDev in DisplayedPages then
      try
        GetDeviceInfo;
      except
        {$IFDEF DEBUG}
        Error('GetDeviceInfo');
        {$ENDIF}
      end;
    if pgEng in DisplayedPages then
      try
        GetEngInfo;
      except
        {$IFDEF DEBUG}
        Error('GetEngInfo');
        {$ENDIF}
      end;
    if pgDisk in DisplayedPages then
      try
        GetDriveInfo;
      except
        {$IFDEF DEBUG}
        Error('GetDriveInfo');
        {$ENDIF}
      end;
    if pgTZ in DisplayedPages then
      try
        GetTZInfo;
      except
        {$IFDEF DEBUG}
        Error('GetTZInfo');
        {$ENDIF}
      end;
    if pgPrn in DisplayedPages then
      try
        GetPrintInfo;
      except
        {$IFDEF DEBUG}
        Error('GetPrintInfo');
        {$ENDIF}
      end;
    if pgStartup in DisplayedPages then
      try
        GetStartupInfo;
      except
        {$IFDEF DEBUG}
        Error('GetStartupInfo');
        {$ENDIF}
      end;
    if pgSoftware in DisplayedPages then
      try
        GetSWInfo;
      except
        {$IFDEF DEBUG}
        Error('GetSWInfo');
        {$ENDIF}
      end;
    if pgStorage in DisplayedPages then
      try
        GetStorageInfo;
      except
        {$IFDEF DEBUG}
        Error('GetStorageInfo');
        {$ENDIF}
      end;
    if pgUSB in DisplayedPages then
      try
        GetUSBInfo;
      except
        {$IFDEF DEBUG}
        Error('GetUSBInfo');
        {$ENDIF}
      end;
    if pgProcesses in DisplayedPages then
      try
        GetProcessesInfo;
      except
        {$IFDEF DEBUG}
        Error('GetProcessesInfo');
        {$ENDIF}
      end;
    if pgServices in DisplayedPages then
      try
        GetServicesInfo;
      except
        {$IFDEF DEBUG}
        Error('GetServicesInfo');
        {$ENDIF}
      end;
    (*
    if pgDrivers in DisplayedPages then
      try
        GetDriversInfo;
      except
        {$IFDEF DEBUG}
        Error('GetDriversInfo');
        {$ENDIF}
      end;
    *)
    if pgSecurity in DisplayedPages then
      try
        GetSCInfo;
      except
        {$IFDEF DEBUG}
        Error('GetSCInfo');
        {$ENDIF}
      end;
    if pgWIFI in DisplayedPages then
      try
        GetWIFIInfo;
      except
        {$IFDEF DEBUG}
        Error('GetWIFIInfo');
        {$ENDIF}
      end;
    if pgBluetooth in DisplayedPages then
      try
        GetBTInfo;
      except
        {$IFDEF DEBUG}
        Error('GetBTInfo');
        {$ENDIF}
      end;
    if tsEL.TabVisible then
      try
        GetELInfo;
      except
        {$IFDEF DEBUG}
        Error('GetELInfo');
        {$ENDIF}
      end;
    if tsAD.TabVisible then
      try
        GetADInfo;
      except
        {$IFDEF DEBUG}
        Error('GetADInfo');
        {$ENDIF}
      end;
  finally
    screen.cursor:=crdefault;
  end;
  pc.activepage:=tsWksta;
end;

procedure Tdlg_msi_Overview.FormCreate(Sender: TObject);
var
  g: TGUID;
  ii: integer;
  h: HICON;
begin
  sl:=TStringList.Create;
  DisplayedPages:=pgAll;
  pcCPU.ActivePage:=tsID;
  pcMachine.ActivePage:=tsMachineGeneral;
  pcSM.ActivePage:=tsBIOS;
  pcOS.ActivePage:=tsGeneral;
  pcEng.ActivePage:=tsEng;
  pcDev.ActivePage:=tsDevTree;
  pcAPM.ActivePage:=tsBat;
  pcNetEnv.ActivePage:=tsNEConns;
  pcNet.ActivePageIndex:=0;
  pcUSB.ActivePageIndex:=0;
  pcWiFi.ActivePageIndex:=0;
  pcSec.ActivePageIndex:=0;

  spid.cbSize:=sizeof(spid);
  SetupDiGetClassImageList(spid);
  ilSystem.Handle:=spid.ImageList;

  g:=GUID_DEVCLASS_COMPUTER;
  SetupDiLoadClassIcon(g,h,ii);
  icoComputer.Picture.Icon.Handle:=h;
  icoMachine.Picture.Icon.Handle:=h;

  g:=GUID_DEVCLASS_PROCESSOR;
  SetupDiLoadClassIcon(g,h,ii);
  icoCPU.Picture.Icon.Handle:=h;
  icoProc.Picture.Icon.Handle:=h;

  g:=GUID_DEVCLASS_MTD;
  SetupDiLoadClassIcon(g,h,ii);
  icoMemory.Picture.Icon.Handle:=h;

  g:=GUID_DEVCLASS_DISPLAY;
  SetupDiLoadClassIcon(g,h,ii);
  icoVideo.Picture.Icon.Handle:=h;

  g:=GUID_DEVCLASS_MONITOR;
  SetupDiLoadClassIcon(g,h,ii);
  icoMonitor.Picture.Icon.Handle:=h;

  g:=GUID_DEVCLASS_BATTERY;
  SetupDiLoadClassIcon(g,h,ii);
  icoBattery.Picture.Icon.Handle:=h;

  g:=GUID_DEVCLASS_NET;
  SetupDiLoadClassIcon(g,h,ii);
  icoNet.Picture.Icon.Handle:=h;

  {$IFDEF THEMESUPPORT}
  pNumLock.ParentBackground:=False;
  pCapsLock.ParentBackground:=False;
  pScrollLock.ParentBackground:=False;
  pAV.ParentBackground:=False;
  pAS.ParentBackground:=False;
  pFW.ParentBackground:=False;
  {$ENDIF}
end;

procedure Tdlg_msi_Overview.cmRefresh(Sender: TObject);
begin
  RefreshData(True);
end;

procedure Tdlg_msi_Overview.cmClose(Sender: TObject);
begin
  close;
end;

procedure Tdlg_msi_Overview.cmCaps(Sender: TObject);
var
  i :integer;
  sl :TStringList;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    Notebook.pageindex:=1;
    sl:=TStringList.Create;
    case TComponent(sender).tag of
      0: begin
        TabSheet1.Caption:='Curve Capabilities';
        GetCurveCapsStr(SysInfo.Display.CurveCaps,sl);
      end;
      1: begin
        TabSheet1.Caption:='Line Capabilities';
        GetLineCapsStr(SysInfo.Display.LineCaps,sl);
      end;
      2: begin
        TabSheet1.Caption:='Polygonal Capabilities';
        GetPolygonCapsStr(SysInfo.Display.PolygonCaps,sl);
      end;
      3: begin
        TabSheet1.Caption:='Raster Capabilities';
        GetRasterCapsStr(SysInfo.Display.RasterCaps,sl);
      end;
      4: begin
        TabSheet1.Caption:='Text Capabilities';
        GetTextCapsStr(SysInfo.Display.TextCaps,sl);
      end;
      5: begin
        TabSheet1.Caption:='Shade Blend Capabilities';
        GetShadeBlendCapsStr(SysInfo.Display.ShadeBlendCaps,sl);
      end;
      6: begin
        TabSheet1.Caption:='Color Management Capabilities';
        GetColorMgmtCapsStr(SysInfo.Display.ColorMgmtCaps,sl);
      end;
    end;
    clb.items.clear;
    for i:=0 to sl.count-1 do begin
      clb.items.Add(sl.Names[i]);
      clb.Checked[clb.items.count-1]:=Boolean(StrToInt(sl.Values[sl.Names[i]]));
    end;
    sl.free;
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.cmPrintSetup(Sender: TObject);
begin
  psd.execute;
end;

procedure Tdlg_msi_Overview.cbDisplayChange(Sender: TObject);
begin
  eChip.text:='';
  eDAC.text:='';
  eBIOSString.text:='';
  eMem.text:='';
  if cbDisplay.ItemIndex>-1 then
    with SysInfo.Display.Adapter[cbDisplay.ItemIndex] do begin
      eChip.text:=Chipset;
      eDAC.text:=DAC;
      eBIOSString.text:=BIOS;
      eMem.text:=IntToStr(Memory);
    end;
end;

procedure Tdlg_msi_Overview.cbDriveChange(Sender: TObject);
var
  p,i :Word;
  b :string;
  sl :TStringList;
  rh: Boolean;
begin
  with SysInfo.Disk do begin
    gdisk.Position:=0;
    p:=0;
    if cbDrive.Text='' then
      Exit;
    Drive:=copy(cbDrive.text,1,2);
    if not Livedata then begin
      rh:=True;
      LoadFromStorage(StorageFilename,rh,SysInfo.StreamCodeProc);
    end;
    b:=Drive+'\';
    lDriveType.caption:=GetMediaTypeStr(MediaType)+' - '+FileSystem;
    if MediaPresent then
      imgDrive.picture.icon.handle:=extractassociatedicon(hinstance,PChar(b),p)
    else
      imgDrive.picture.icon.handle:=0;
    eUNC.text:=UNCPath;
    eDSN.text:=SerialNumber;
    if pos('[',cbdrive.items[cbdrive.itemindex])=0 then begin
      i:=cbdrive.itemindex;
      cbdrive.items[i]:=cbdrive.items[i]+' ['+VolumeLabel+']';
      cbdrive.itemindex:=i;
    end;
    lCapacity.caption:=formatfloat('Capacity: #,#0 B',Capacity);
    lFree.caption:=formatfloat('Free space: #,#0 B',FreeSpace);
    if Capacity>0 then
      gDisk.Position:=round((Capacity-FreeSpace)/Capacity*100)
    else
      gDisk.Position:=0;
    lBPS.caption:=formatfloat('Bytes/sector: 0',BytesPerSector);
    lSPC.caption:=formatfloat('Sector/cluster: 0',SectorsPerCluster);
    lFC.caption:=formatfloat('Free clusters: #,#0',FreeClusters);
    lTC.caption:=formatfloat('Total clusters: #,#0',TotalClusters);
    sl:=TStringList.Create;
    GetFileFlagsStr(sl);
    clbFlags.items.Clear;
    for i:=1 to sl.count-1 do begin
      clbFlags.items.Add(sl.Names[i]);
      clbFlags.Checked[clbFlags.items.count-1]:=Boolean(StrToInt(sl.Values[sl.Names[i]]));
    end;
    sl.Free;
  end;
end;

procedure Tdlg_msi_Overview.clbBIOSClickCheck(Sender: TObject);
begin
  clbBIOS.Checked[clbBIOS.ItemIndex]:=not clbBIOS.Checked[clbBIOS.ItemIndex];
end;

procedure Tdlg_msi_Overview.clbClickCheck(Sender: TObject);
var
  OCC: TNotifyEvent;
  idx: integer;
  p: TPoint;
begin
  with TCheckListBox(Sender) do begin
    OCC:=OnClickCheck;
    OnClickCheck:=nil;
    GetCursorPos(p);
    p:=ScreenToClient(p);
    idx:=ItemAtPos(p,True);
    if idx>-1 then
      Checked[idx]:=not Checked[idx];
    OnClickCheck:=OCC;
  end;
end;

procedure Tdlg_msi_Overview.SeTInfoPages(const Value: TInfoPages);
var
  i: integer;
begin
  FPages:=Value;
  for i:=pc.PageCount-1 downto 0 do begin
    pc.Pages[i].TabVisible:=TInfoPage(i) in DisplayedPages;
    if pc.Pages[i].TabVisible then
      pc.ActivePage:=pc.Pages[i];
  end;
end;

procedure Tdlg_msi_Overview.cmModes(Sender: TObject);
var
  i: integer;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    Notebook.pageindex:=1;
    clb.items.clear;
    clb.Items.AddStrings(SysInfo.Display.Modes);
    for i:=0 to clb.Items.Count-1 do
      clb.Checked[i]:=True;
    TabSheet1.Caption:='Supported Video Modes';
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.GetTZInfo;
begin
  with SysInfo.OS.TimeZone do begin
    eTZ.Text:=DisplayName;
    gbStd.Caption:=' '+StandardName+' ';
    gbDay.Caption:=' '+DaylightName+' ';
    eStdStart.Text:=DateTimeToStr(StandardStart);
    eStdBias.Text:=IntToStr(StandardBias);
    eDayStart.Text:=DateTimeToStr(DaylightStart);
    eDayBias.Text:=IntToStr(DaylightBias);
  end;
end;

procedure Tdlg_msi_Overview.cmProto(Sender: TObject);
var
  i: integer;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    Notebook.pageindex:=1;
    clb.items.clear;
    clb.Items.AddStrings(SysInfo.Network.Protocols);
    for i:=0 to clb.Items.Count-1 do
      clb.Checked[i]:=True;
    TabSheet1.Caption:='Protocols';
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.cmServ(Sender: TObject);
var
  i: integer;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    Notebook.pageindex:=1;
    clb.items.clear;
    clb.Items.AddStrings(SysInfo.Network.Services);
    for i:=0 to clb.Items.Count-1 do
      clb.Checked[i]:=True;
    TabSheet1.Caption:='Services';
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.cmCli(Sender: TObject);
var
  i: integer;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    Notebook.pageindex:=1;
    clb.items.clear;
    clb.Items.AddStrings(SysInfo.Network.Clients);
    for i:=0 to clb.Items.Count-1 do
      clb.Checked[i]:=True;
    TabSheet1.Caption:='Clients';
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.SetCaptionText(const Value: string);
begin
  Caption:=Value;
end;

procedure Tdlg_msi_Overview.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  sl.Free;
  for i:=0 to cbCPU.Items.Count-1 do
    if Assigned(cbCPU.Items.Objects[i]) then
      dispose(PInteger(cbCPU.Items.Objects[i]));
  Action:=caFree;
end;

procedure Tdlg_msi_Overview.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  bProps.Enabled:=Assigned(Node) and (Node.Level=2);
  bRes.Enabled:=bProps.Enabled;
end;

procedure Tdlg_msi_Overview.cmProps(Sender: TObject);
var
  dr: TDevice;
  i: integer;
begin
  if Assigned(Tree.Selected) and (Tree.Selected.Level=2) then
    with Tdlg_msi_Detail.Create(self) do begin
      Notebook.pageindex:=3;
      lv.items.clear;
      i:=PInteger(Tree.Selected.Data)^;
      dr:=SysInfo.Devices.Devices[i];
      with lv.Items.Add do begin
        Caption:='Device Name';
        Subitems.Add(Tree.Selected.Text);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Class Name';
        Subitems.Add(dr.ClassName);
      end;
      with lv.Items.Add do begin
        Caption:='Class Description';
        Subitems.Add(Tree.Selected.Parent.Text);
      end;
      with lv.Items.Add do begin
        Caption:='Class GUID';
        Subitems.Add(dr.GUID);
      end;
      with lv.Items.Add do begin
        Caption:='Manufacturer';
        Subitems.Add(dr.Manufacturer);
      end;
      with lv.Items.Add do begin
        Caption:='Hardware ID';
        Subitems.Add(dr.HardwareID);
      end;
      with lv.Items.Add do begin
        Caption:='SymbolicLink';
        Subitems.Add(dr.SymbolicLink);
      end;
      with lv.Items.Add do begin
        Caption:='Location';
        SubItems.Add(dr.Location);
      end;
      with lv.Items.Add do begin
        Caption:='PCI Number';
        Subitems.Add(Format('%d',[dr.PCINumber]));
      end;
      with lv.Items.Add do begin
        Caption:='Device Number';
        Subitems.Add(Format('%d',[dr.DeviceNumber]));
      end;
      with lv.Items.Add do begin
        Caption:='Function Number';
        Subitems.Add(Format('%d',[dr.FunctionNumber]));
      end;
      with lv.Items.Add do begin
        Caption:='Vendor ID';
        Subitems.Add(Format('%4.4x',[dr.VendorID]));
      end;
      with lv.Items.Add do begin
        Caption:='Device ID';
        Subitems.Add(Format('%4.4x',[dr.DeviceID]));
      end;
      with lv.Items.Add do begin
        Caption:='SubSystem';
        Subitems.Add(Format('%8.8x',[dr.SubSysID]));
      end;
      with lv.Items.Add do begin
        Caption:='Revision';
        Subitems.Add(Format('%2.2x',[dr.Revision]));
      end;
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Driver Description';
        SubItems.Add(dr.Driver);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Driver Version';
        SubItems.Add(dr.DriverVersion);
      end;
      with lv.Items.Add do begin
        Caption:='Driver Date';
        SubItems.Add(dr.DriverDate);
      end;
      with lv.Items.Add do begin
        Caption:='Driver Provider';
        SubItems.Add(dr.DriverProvider);
      end;
      with lv.Items.Add do begin
        Caption:='Driver GUID';
        SubItems.Add(dr.DriverKey);
      end;
      with lv.Items.Add do begin
        Caption:='Image Path';
        SubItems.Add(dr.ImagePath);
      end;
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Service Name';
        if dr.ServiceName='' then
          SubItems.Add(dr.Service)
        else
          SubItems.Add(dr.ServiceName);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Service Group';
        SubItems.Add(dr.ServiceGroup);
      end;
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Install ID';
        SubItems.Add(dr.InstallID);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Install Date';
        SubItems.Add(DateTimeToStrDef(dr.InstallDate));
      end;
      with lv.Items.Add do begin
        Caption:='First Install Date';
        SubItems.Add(DateTimeToStrDef(dr.FirstInstallDate));
      end;
      with lv.Items.Add do begin
        Caption:='Last Arrival Date';
        SubItems.Add(DateTimeToStrDef(dr.LastArrivalDate));
      end;
      with lv.Items.Add do begin
        Caption:='Last Removal Date';
        SubItems.Add(DateTimeToStrDef(dr.LastRemovalDate));
      end;
      TabSheet1.Caption:='Device Properties';
      showmodal;
      free;
    end;
end;

procedure Tdlg_msi_Overview.GetPrintInfo;
var
  i: integer;
begin
  with SysInfo.Printers do begin
    bPrint.Visible:=LiveData;
    lvPrinter.items.clear;
    for i:=0 to PrinterCount-1 do
      with lvPrinter.items.add do begin
        caption:=PrinterName[i];
        SubItems.Add(Driver[i]);
        SubItems.Add(Port[i]);
        if Pos('\\',Port[i])>0 then
          ImageIndex:=2
        else
          ImageIndex:=0;
        if SameText(PrinterName[i],DefaultPrinter) then
          ImageIndex:=ImageIndex+1;
      end;
  end;
end;

procedure Tdlg_msi_Overview.tcStartupChange(Sender: TObject);
var
  i: integer;
begin
  with SysInfo, lvStartup, Items do begin
    BeginUpdate;
    Clear;
    case tcStartup.TabIndex of
      0: begin
        for i:=0 to Startup.Count-1 do
          if Startup.Records[i].Location=arlFolder then
            with lvStartup.Items.Add do begin
              Caption:=Startup.Records[i].Name;
              SubItems.Add(Startup.Records[i].Path);
              SubItems.Add(Startup.Records[i].CmdLine);
              ImageIndex:=-1;
            end;
      end;
      1: begin
        for i:=0 to Startup.Count-1 do
          if Startup.Records[i].Location=arlRegistry then
            with lvStartup.Items.Add do begin
              Caption:=Startup.Records[i].Name;
              SubItems.Add(Startup.Records[i].Path);
              SubItems.Add(Startup.Records[i].CmdLine);
              ImageIndex:=-1;
            end;
      end;
      2: begin
        for i:=0 to Startup.Count-1 do
          if Startup.Records[i].Location=arlFile then
            with lvStartup.Items.Add do begin
              Caption:=Startup.Records[i].Name;
              SubItems.Add(Startup.Records[i].Path);
              SubItems.Add(Startup.Records[i].CmdLine);
              ImageIndex:=-1;
            end;
      end;
      3: begin
        for i:=0 to Startup.Count-1 do
          if Startup.Records[i].Location=arlBHO then
            with lvStartup.Items.Add do begin
              Caption:=Startup.Records[i].Name;
              SubItems.Add(Startup.Records[i].Path);
              SubItems.Add(Startup.Records[i].CmdLine);
              ImageIndex:=-1;
            end;
      end;
      4: begin
        for i:=0 to Startup.Count-1 do
          if Startup.Records[i].Location=arlTaskScheduler then
            with lvStartup.Items.Add do begin
              Caption:=Startup.Records[i].Name;
              SubItems.Add(Startup.Records[i].Path);
              SubItems.Add(Startup.Records[i].CmdLine);
              ImageIndex:=-1;
            end;
      end;
    end;
    EndUpdate;
  end;
end;

procedure Tdlg_msi_Overview.GetSCInfo;
var
  i: Integer;
  s: string;
begin
  lvAV.Items.Clear;
  lvAS.Items.Clear;
  lvFW.Items.Clear;
  with SysInfo.Security do begin
    for i:=0 to AntiVirus.Count-1 do
      with lvAV.Items.Add do begin
        Caption:=ListName(AntiVirus,i);
        SubItems.Add(ListValueFromIndex(AntiVirus,i));
      end;
    for i:=0 to AntiSpyware.Count-1 do
      with lvAS.Items.Add do begin
        Caption:=ListName(AntiSpyware,i);
        SubItems.Add(ListValueFromIndex(AntiSpyware,i));
      end;
    for i:=0 to Firewall.Count-1 do
      with lvFW.Items.Add do begin
        Caption:=ListName(Firewall,i);
        SubItems.Add(ListValueFromIndex(Firewall,i));
      end;
  end;

  with SysInfo.Firewall do begin
    cbxDomain.Checked:=DomainProfile;
    cbxPublic.Checked:=PublicProfile;
    cbxPrivate.Checked:=PrivateProfile;
    lvWFW.Items.Clear;
    for i:=0 to RuleCount-1 do
      with lvWFW.Items.Add do begin
        Caption:=Rules[i].Name;
        SubItems.Add(Rules[i].Description);
        SubItems.Add(Rules[i].AppName);
        SubItems.Add(Rules[i].ServiceName);
        case Rules[i].Protocol of
          NET_FW_IP_PROTOCOL_TCP    :s:='TCP';
          NET_FW_IP_PROTOCOL_UDP    :s:='UDP';
          NET_FW_IP_PROTOCOL_ICMPv4 :s:='ICMPv4';
          NET_FW_IP_PROTOCOL_ICMPv6 :s:='ICMPv6';
          else s:=IntToStr(Rules[i].Protocol);
        end;
        SubItems.Add(s);
        SubItems.Add(Rules[i].LocalPorts);
        SubItems.Add(Rules[i].RemotePorts);
        SubItems.Add(Rules[i].LocalAddresses);
        SubItems.Add(Rules[i].RemoteAddresses);
        SubItems.Add(Rules[i].ICMP);
        case Rules[i].Direction of
          NET_FW_RULE_DIR_IN : s:='In';
          NET_FW_RULE_DIR_OUT: s:='Out';
        end;
        SubItems.Add(s);
        SubItems.Add(BoolToStr(Rules[i].Enabled,True));
        SubItems.Add(BoolToStr(Rules[i].Edge,True));
        case Rules[i].Action of
          NET_FW_ACTION_ALLOW: s:='Allow';
          NET_FW_ACTION_BLOCk: s:='Block';
        end;
        SubItems.Add(Rules[i].Grouping);
        SubItems.Add(Rules[i].IntfTypes);
      end;
  end;
end;

procedure Tdlg_msi_Overview.GetServicesInfo;
var
  i: Integer;
begin
  with SysInfo.ProcessList do begin
    try
      SvcList.Items.BeginUpdate;
      SvcList.Items.Clear;
      for i:=0 to ServiceCount-1 do
        with SvcList.Items.Add do begin
          Caption:=Services[i].DisplayName;
          SubItems.Add(cSvcType[Services[i].Typ]);
          SubItems.Add(cSvcStatus[Services[i].Status]);
          SubItems.Add(cSvcStartup[Services[i].StartUp]);
          SubItems.Add(Services[i].ImageName);
        end;
    finally
      SvcList.Items.EndUpdate;
    end;
  end;
  tsServices.Caption:=Format('Services (%d)',[SysInfo.ProcessList.ServiceCount]);
end;

procedure Tdlg_msi_Overview.GetStartupInfo;
begin
  tcStartupChange(tcStartup);
end;

procedure Tdlg_msi_Overview.GetSWInfo;
var
  i: integer;
begin
  pcSW.ActivePage:=tsSWApp;
  with SysInfo.Software, lvSW, Items do begin
    BeginUpdate;
    Clear;
    for i:=0 to SysInfo.Software.Count-1 do
      with Add do begin
        Caption:=InstallEntry[i].Name;
        SubItems.Add(InstallEntry[i].Version);
        if InstallEntry[i].InstallDate=0 then
          SubItems.Add('')
        else
          SubItems.Add(DateToStr(InstallEntry[i].InstallDate));
        ImageIndex:=-1;
      end;
    EndUpdate;
  end;
  tsSWApp.Caption:=Format('Applications (%d)',[SysInfo.Software.Count]);

  with SysInfo.MSProduct, lvMSP, Items do begin
    BeginUpdate;
    Clear;
    for i:=0 to ProductCount-1 do
      with Add do begin
        Caption:=Products[i].Name;
        SubItems.Add(Products[i].ProductID);
        SubItems.Add(Products[i].ProductKey);
        ImageIndex:=-1;
      end;
    EndUpdate;
  end;
  tsSWMSP.Caption:=Format('MS Products (%d)',[SysInfo.MSProduct.ProductCount]);
end;

procedure Tdlg_msi_Overview.cmNTSpec(Sender: TObject);
var
  sl: TStringList;
  i: integer;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    Notebook.pageindex:=1;
    TabSheet1.Caption:='Installed Suites';
    clb.items.clear;
    sl:=TStringList.Create;
    SysInfo.OS.GetInstalledSuitesStr(sl);
    for i:=0 to sl.count-1 do begin
      clb.items.Add(sl.Names[i]);
      clb.Checked[clb.items.count-1]:=Boolean(StrToInt(sl.Values[sl.Names[i]]));
    end;
    sl.Free;
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.cbxClick(Sender: TObject);
var
  oc: TNotifyEvent;
begin
  with TCheckBox(Sender) do begin
    oc:=OnClick;
    OnClick:=nil;
    Checked:=not Checked;
    OnClick:=oc;
  end;
end;

procedure Tdlg_msi_Overview.FormDestroy(Sender: TObject);
begin
  SetupDiDestroyClassImageList(spid);
  SysInfo:=nil;
end;


procedure Tdlg_msi_Overview.GetStorageInfo;
var
  i,j,ii: Integer;
  n,r: TTreeNode;
  pi: PInteger;
  g: TGUID;
begin
  with SysInfo, Storage do
    try
      Screen.Cursor:=crHourGlass;
      StorageTree.Items.Clear;
      for i:=0 to PhysicalCount-1 do
        with Physical[i] do begin
          New(pi);
          pi^:=i;
          if Size>0 then
            r:=StorageTree.Items.AddChildObject(nil,Trim(Format('%s (%d MB)',[Model,Size shr 20])),pi)
          else
            r:=StorageTree.Items.AddChildObject(nil,Trim(Format('%s',[Model])),pi);

          g:=GUID_DEVCLASS_DISKDRIVE;
          case DeviceType of
            FILE_DEVICE_CD_ROM,
            FILE_DEVICE_DVD: g:=GUID_DEVCLASS_CDROM;
            FILE_DEVICE_TAPE: g:=GUID_DEVCLASS_TAPEDRIVE;
            FILE_DEVICE_DISK: if Removable then
                                 g:=GUID_DEVCLASS_FLOPPYDISK;
          end;
          SetupDiGetClassImageIndex(spid,g,ii);
          r.ImageIndex:=ii;
          r.SelectedIndex:=r.ImageIndex;
          for j:=0 to LogicalCount-1 do
            with Logical[j] do
              if PhysicalIndex=i then begin
                New(pi);
                pi^:=j;
                if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) and (LayoutIndex>-1) then
                  n:=StorageTree.Items.AddChildObject(r,Format('%s: (%s %s - %d MB)',[
                                   Drive,
                                   GetPartitionType(Layout[LayoutIndex].Number,Layout[LayoutIndex].Typ),
                                   FileSystem,//GetPartitionSystem(Layout[LayoutIndex].Typ),
                                   Layout[LayoutIndex].Length.QuadPart shr 20]),pi)
                else begin
                  Disk.Drive:=Drive+':';
                  if Disk.Capacity=0 then
                    n:=StorageTree.Items.AddChildObject(r,Format('%s:',[Drive]),pi)
                  else
                    n:=StorageTree.Items.AddChildObject(r,Format('%s: (%s - %d MB)',[Drive,Disk.FileSystem,Disk.Capacity shr 20]),pi)
                end;
                g:=GUID_DEVCLASS_VOLUME;
                SetupDiGetClassImageIndex(spid,g,ii);
                n.ImageIndex:=ii;
                n.SelectedIndex:=n.ImageIndex;
              end;
          r.Expand(False);
        end;
    finally
      Screen.Cursor:=crDefault;
    end;
end;

procedure Tdlg_msi_Overview.cmRes(Sender: TObject);
var
  d: TDevice;
  i: Integer;
  DR: TDeviceResources;
  dn: string;
begin
  if Assigned(Tree.Selected) and (Tree.Selected.Level=2) then begin
    i:=PInteger(Tree.Selected.Data)^;
    d:=SysInfo.Devices.Devices[i];
    if SysInfo.Devices.LiveData and  not Empty(d.ResourceListKey) then begin
      Screen.Cursor:=crHourGlass;
      try
        GetDeviceResources(d,DR);
      finally
        Screen.Cursor:=crDefault;
      end;
      dn:=d.Name;
      ShowResourcesDlg(dn,DR);
    end else
      MessageDlg('No resource list available.',mtInformation,[mbOK],0);
  end;
end;

procedure Tdlg_msi_Overview.GetUSBInfo;
var
  ii,i,j: Integer;
  s: string;
  pi: PInteger;
  r,n,c: TTreeNode;
  g: TGUID;
begin
  with SysInfo.USB do begin
    USBTree.Items.BeginUpdate;
    try
      USBTree.Items.Clear;
      for i:=0 to SysInfo.USB.USBNodeCount-1 do
        with SysInfo.USB.USBNodes[i] do begin
          s:='';
          if ClassGUID.D1=0 then
            g:=GUID_DEVCLASS_USB
          else
            g:=ClassGUID;
          SetupDiGetClassImageIndex(spid,g,ii);
          case USBClass of
            usbHostController: s:=s+Format('%s %d',[ClassNames[Integer(USBClass)],USBDevice.Port]);
            usbHub: s:=s+Format('%s (%s)',[USBDevice.USBClassname,ClassNames[Integer(USBClass)]]);
            else begin
              if USBDevice.ConnectionStatus=1 then begin
                if USBClass=usbExternalHub then
                  s:=s+Format('Port[%d]: %s (%s)',[USBDevice.Port,USBDevice.USBClassname,ClassNames[Integer(USBClass)]])
                else begin
                  if USBDevice.Product<>'' then
                    s:=s+Format('Port[%d]: %s',[USBDevice.Port,USBDevice.Product])
                  else
                    s:=s+Format('Port[%d]: %s',[USBDevice.Port,USBDevice.USBClassname]);
                  if IsEqualGUID(g,GUID_DEVCLASS_USB) and (Length(USBDevice.Registry)>0) then begin
                    g:=USBDevice.Registry[0].DeviceClassGUID;
                    SetupDiGetClassImageIndex(spid,g,ii);
                  end;
                end;
              end else
                s:=s+Format('Port[%d]: %s',[USBDevice.Port,ConnectionStates[USBDevice.ConnectionStatus]]);
            end;
          end;
          r:=FindUSBNode(ParentIndex);
          new(pi);
          pi^:=i;
          n:=USBTree.Items.AddChildObject(r,s,pi);
          n.ImageIndex:=ii;
          n.SelectedIndex:=n.ImageIndex;
          if Assigned(r) then
            r.Expand(False);
          r:=n;
          if (USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError]) and (USBDevice.ConnectionStatus=1) then begin
            if Length(USBdevice.Registry)>0 then begin
              for j:=0 to High(USBDevice.Registry) do begin
                g:=USBDevice.Registry[j].DeviceClassGUID;
                SetupDiGetClassImageIndex(spid,g,ii);
                new(pi);
                pi^:=MakeWord(j,i+1);
                n:=USBTree.Items.AddChildObject(r,USBDevice.Registry[j].DeviceClass,pi);
                n.ImageIndex:=ii;
                n.SelectedIndex:=n.ImageIndex;
                if (USBDevice.Registry[j].Drive<>'') and USBDevice.Registry[j].DriveConnected then begin
                  new(pi);
                  pi^:=MakeWord(j,i+1);
                  c:=USBTree.Items.AddChildObject(n,Format('Drive: %s:',[USBDevice.Registry[j].Drive]),pi);
                  g:=GUID_DEVCLASS_VOLUME;
                  SetupDiGetClassImageIndex(spid,g,ii);
                  c.ImageIndex:=ii;
                  c.SelectedIndex:=c.ImageIndex;
                end;
              end;
            end;
          end;
        end;
    finally
      USBTree.Items.EndUpdate;
    end;
  end;

  for i:=0 to SysInfo.USBHistory.RecordCount-1 do
    with lvUSBH.Items.Add do begin
      Caption:=SysInfo.USBHistory.Records[i].Name;
      Subitems.Add(SysInfo.USBHistory.Records[i].SerialNumber);
      SubItems.Add(DateTimeToStr(SysInfo.USBHistory.Records[i].Timestamp));
      Subitems.Add(SysInfo.USBHistory.Records[i].DeviceClass);
    end;
  lvUSBH.AlphaSort;
end;

procedure Tdlg_msi_Overview.USBTreeChange(Sender: TObject; Node: TTreeNode);

procedure AddItem(const AProperty,AValue: string);
begin
  with lvUSB.Items.Add do begin
    Caption:=AProperty;
    SubItems.Add(AValue);
  end;
end;

var
  s: string;
  pi: integer;
begin
  lvUSB.Items.BeginUpdate;
  try
    lvUSB.Items.Clear;
    if Assigned(USBTree.Selected) and Assigned(USBTree.Selected.Data) then begin
      pi:=PInteger(USBTree.Selected.Data)^;
      if pi<256 then begin
        with SysInfo.USB.USBNodes[pi] do
          if (USBDevice.ConnectionStatus=1) then begin
            if USBDevice.USBClassname='' then
              AddItem('Class',ClassNames[Integer(USBClass)])
            else
              AddItem('Class',USBDevice.USBClassName);
            AddItem('Manufacturer',USBDevice.Manufacturer);
            if (USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError]) then begin
              AddItem('ClassGUID',GUIDToString(ClassGUID));
              AddItem('Connection Name',ConnectionName);
              AddItem('Serial',USBDevice.Serial);
              AddItem('Power consumption',Format('%d mA',[USBDevice.MaxPower]));
              case SysInfo.USB.GetDevicePowerState(DeviceInstanceId,Keyname) of
                PowerDeviceUnspecified: s:='Unspecified';
                PowerDeviceD0: s:='D0';
                PowerDeviceD1: s:='D1';
                PowerDeviceD2: s:='D2';
                PowerDeviceD3: s:='D3';
              end;
              AddItem('Power state',s);
              AddItem('Specification version',Format('%d.%d',[USBDevice.MajorVersion,USBDevice.MinorVersion]));
              AddItem('Driver key',Keyname);
              AddItem('Last init',DateTimeToStr(TimeStamp));
            end;
          end;
      end else
        with SysInfo.USB.USBNodes[Hi(pi)-1] do begin
          AddItem('Class',USBDevice.Registry[Lo(pi)].DeviceClass);
          AddItem('Name',USBDevice.Registry[Lo(pi)].Name);
          AddItem('ClassGUID',GUIDToString(USBDevice.Registry[Lo(pi)].DeviceClassGUID));
          AddItem('Last init',DateTimeToStr(USBDevice.Registry[Lo(pi)].Timestamp));
        end;
    end;
  finally
    lvUSB.Items.EndUpdate;
  end;
end;

procedure Tdlg_msi_Overview.USBTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  n: TUSBNode;
begin
  if Assigned(Node) then begin
    if Assigned(Node.Data) and (PInteger(Node.Data)^<256) then begin
      n:=SysInfo.USB.USBNodes[PInteger(Node.Data)^];
      if n.USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError] then begin
        if n.USBDevice.ConnectionStatus=1 then begin
          Sender.Canvas.Font.Style:=[fsBold];
          if (cdsSelected in State) and not Sender.Focused then
            Sender.Canvas.Font.Color:=clWindowText;
        end else
          Sender.Canvas.Font.Color:=clGray;
      end;
    end;
  end;
end;

procedure Tdlg_msi_Overview.TreeDblClick(Sender: TObject);
begin
  cmProps(nil);
end;

procedure Tdlg_msi_Overview.TreeDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    try Dispose(PInteger(Node.Data)) except end;
end;

procedure Tdlg_msi_Overview.tvADChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
  s: string;
  ado: TADObject;
begin
  lvAD.Items.BeginUpdate;
  with SysInfo.ActiveDirectory do
  try
    lvAD.Items.Clear;
    if not Assigned(Node) then
      Exit;
    lvAD.SortType:=stText;
    case Node.ImageIndex of
      1: with lvAD.Items.Add do begin
           Caption:='Count';
           SubItems.Add(IntToStr(Node.Count));
         end;
      0,2..4: begin
         case Node.ImageIndex of
           0: ado:=Domain;
           2: ado:=Users[PInteger(Node.Data)^];
           3: ado:=Computers[PInteger(Node.Data)^];
           4: ado:=Groups[PInteger(Node.Data)^];
         end;
         with ado do
           for i:=0 to High(Props) do
             with lvAD.Items.Add do begin
               Caption:=Props[i].Name;
               s:=Props[i].Value;
               if Props[i].Typ=ADSTYPE_LARGE_INTEGER then
                 s:=Format('%s = %s',[s,DateTimeToStr(Int64ToDatetime(Props[i].Value))]);
               SubItems.Add(s);
             end;
      end;
    end;
    pAD.Caption:=Format('Users: %d   Computers: %d   Groups: %d',[UserCount,ComputerCount,GroupCount]);
  finally
    lvAD.Items.Endupdate;
  end;
end;

procedure Tdlg_msi_Overview.tvADDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    Dispose(PInteger(Node.Data));
end;

function Tdlg_msi_Overview.FindUSBNode(AIndex: Integer): TTreeNode;
var
  n: TTreeNode;
begin
  Result:=nil;
  n:=USBTree.Items.GetFirstNode;
  while Assigned(n) do begin
    if Assigned(n.Data) and (PInteger(n.Data)^=AIndex) then begin
      Result:=n;
      Break;
    end;
    n:=n.GetNext;
  end;
end;

procedure Tdlg_msi_Overview.StorageTreeCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Assigned(Node) then
    if (Node.Level=0) then begin
      Sender.Canvas.Font.Style:=[fsBold];
      if (cdsSelected in State) and not Sender.Focused then
        Sender.Canvas.Font.Color:=clWindowText;
    end;
end;

procedure Tdlg_msi_Overview.GetProcessesInfo;
var
  i: Integer;
begin
  with SysInfo.ProcessList do begin
    try
      ProcList.Items.BeginUpdate;
      ProcList.Items.Clear;
      for i:=0 to ProcessCount-1 do
        with ProcList.Items.Add do begin
          Caption:=Processes[i].Name;
          SubItems.Add(Format('%d',[Processes[i].PID]));
          SubItems.Add(Format('%dbit',[Processes[i].Bits]));
          SubItems.Add(FormatTicks(Processes[i].CPUTimes.UserTime.QuadPart+Processes[i].CPUTimes.KernelTime.QuadPart));
          SubItems.Add(Format('%d KB',[Processes[i].VMCounters.WorkingSetSize div 1024]));
          SubItems.Add(Processes[i].ImageName);
        end;
    finally
      ProcList.Items.EndUpdate;
    end;
  end;
  tsProcesses.Caption:=Format('Processes (%d)',[SysInfo.ProcessList.ProcessCount]);
end;

procedure Tdlg_msi_Overview.RefreshData(ForceRefresh: Boolean);
begin
  screen.cursor:=crhourglass;
  try
    if ForceRefresh then begin
      if SysInfo.LiveData then
        SysInfo.RefreshData
      else
        LoadFromSIF(SysInfo);
    end;
    DisplayData;
  finally
    screen.cursor:=crdefault;
  end;
end;

procedure Tdlg_msi_Overview.GetMonitorInfo;
var
  i,idx: integer;
begin
  idx:=0;
  with SysInfo, Monitor do begin
    cbMon.Items.Clear;
    for i:=0 to Count-1 do
      cbMon.Items.Add(Monitors[i].DeviceDescription);
    cbMon.ItemIndex:=0;
    cbMonChange(nil);
  end;
end;

procedure Tdlg_msi_Overview.cbMonChange(Sender: TObject);
begin
  with SysInfo, Monitor.Monitors[cbMon.ItemIndex] do begin
    eMonName.Text:=EDID.Name;
    eMonPN.Text:=EDID.ProductNumber;
    eMonMan.Text:=Manufacturer;
    eMonDesc.Text:=DeviceDescription;
    eMonSize.Text:=Format('(%d x %d) cm',[EDID.Width,EDID.Height]);
    eMonRes.Text:=Format('(%d x %d) px',[RectWidth(Bounds),RectHeight(Bounds)]);
    eWorkArea.Text:=Format('(%d,%d,%d,%d)',[WorkArea.Left,WorkArea.Top,WorkArea.Right,WorkArea.Bottom]);
    eMonSN.Text:=Format('%d',[EDID.SerialNumber]);
    eDPI.Text:=Format('%d',[DPI]);
    eGamma.Text:=Format('%1.2f',[EDID.Gamma]);
    eMonDate.Text:=Format('%d/%d',[EDID.Year,EDID.Week]);
    eEDID.Text:=EDID.Version;
    eMonMod.Text:=Model;
    cbxMonPrim.onClick:=nil;
    cbxMonPrim.Checked:=Primary;
    cbxMonPrim.onClick:=cbxClick;
  end;
end;

procedure Tdlg_msi_Overview.cbProcChange(Sender: TObject);
begin
  if cbProc.ItemIndex=-1 then
    Exit;
  with SysInfo.APM.ProcessorPowerStatus[cbProc.ItemIndex] do begin
    eMaxFreq.Text:=Format('%d MHz',[MaxMHz]);
    eFreq.Text:=Format('%d MHz',[CurrentMHz]);
    eFreqLimit.Text:=Format('%d MHz',[MHzLimit]);
    eMaxIdle.Text:=Format('%d',[MaxIdleState]);
    eIdle.Text:=Format('%d',[CurrentIdleState]);
    pbCPU.Max:=MaxMHz;
    pbCPU.Position:=CurrentMHz;
  end;
end;

procedure Tdlg_msi_Overview.bIntfDetailClick(Sender: TObject);
var
  i: integer;
  a: TAdapter;
begin
  with Tdlg_msi_Detail.Create(self) do begin
    a:=SysInfo.Network.TCPIP.Adapter[lvIntf.Selected.Index];
    Notebook.pageindex:=3;
    lv.items.clear;
    with lv.Items.Add do begin
      Caption:='Alias';
      SubItems.Add(a.Alias);
    end;
    with lv.Items.Add do begin
      Caption:='MAC Address';
      SubItems.Add(a.Address);
    end;
    with lv.Items.Add do begin
      Caption:='MTU';
      SubItems.Add(IntToStr(a.MTU));
    end;
    with lv.Items.Add do begin
      Caption:='Link speed';
      SubItems.Add(Format('%d Mbps',[a.MaxSpeed div 1000000]));
    end;
    with lv.Items.Add do begin
      Caption:='Type';
      SubItems.Add(AdapterTypes[a.Typ]);
    end;
    with lv.Items.Add do begin
      Caption:='DNS connection suffix';
      SubItems.Add(a.DNSSuffix);
    end;
    with lv.Items.Add do begin
      Caption:='DHCP Enabled';
      SubItems.Add(BoolToStr(a.EnableDHCP,True));
    end;
    with lv.Items.Add do begin
      Caption:='IPv4';
      SubItems.Add(a.IPAddress.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv4 Mask';
      SubItems.Add(a.IPAddressMask.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv4 DHCP Servers';
      SubItems.Add(a.DHCP_IPAddress.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv4 Default Gateway';
      SubItems.Add(a.Gateway_IPAddress.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv4 DNS Servers';
      SubItems.Add(a.DNSServers.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv4 WINS Servers';
      SubItems.Add(a.PrimaryWINS_IPAddress.CommaText);
    end;

    with lv.Items.Add do begin
      Caption:='IPv6';
      SubItems.Add(a.IPv6Address.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv6 DHCP Servers';
      SubItems.Add(a.DHCP_IPv6.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv6 Default Gateway';
      SubItems.Add(a.Gateway_IPv6.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv6 DNS Servers';
      SubItems.Add(a.DNSServers_IPv6.CommaText);
    end;
    with lv.Items.Add do begin
      Caption:='IPv6 WINS Servers';
      SubItems.Add(a.PrimaryWINS_IPv6.CommaText);
    end;
    TabSheet1.Caption:=a.Name;
    showmodal;
    free;
  end;
end;

procedure Tdlg_msi_Overview.cbBatChange(Sender: TObject);
var
  s: string;
begin
  eBatCap.Text:='';
  eBatRemain.Text:='';
  eBatRate.Text:='';
  eBatLife.Text:='';
  pbBat.Position:=0;

  if cbBat.ItemIndex=-1 then
    Exit;
  with SysInfo.APM.Battery[cbBat.ItemIndex] do begin
    if SysInfo.APM.BatteryChargeStatus<>[bsNoBattery] then begin
      s:='';
      if PowerState and BATTERY_CHARGING=BATTERY_CHARGING then
        s:=s+'charging, ';
      if PowerState and BATTERY_CRITICAL=BATTERY_CRITICAL then
        s:=s+'critical, ';
      if PowerState and BATTERY_DISCHARGING=BATTERY_DISCHARGING then
        s:=s+'discharging, ';
      if PowerState and BATTERY_POWER_ON_LINE=BATTERY_POWER_ON_LINE then
        s:=s+'power on-line, ';
      if (DesignedCapacity>0) and (CurrentCapacity<=DesignedCapacity) then
        s:=s+Format('(%d%%), ',[Round(CurrentCapacity/DesignedCapacity*100)]);
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
    end else
      eBat.Text:='No battery present';
  end;
end;

procedure Tdlg_msi_Overview.cbELChange(Sender: TObject);
var
  i: Integer;
begin
  if cbEL.ItemIndex=-1 then
    Exit;

  Screen.Cursor:=crHourglass;
  try
    SysInfo.EventLog.ExpandMessages:=False;
    SysInfo.EventLog.SourceName:=cbEL.Text;

    with lvEL.Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to SysInfo.EventLog.RecordCount-1 do
          with Add do begin
            Caption:=DatetimeToStr(SysInfo.EventLog.Records[i].DateTime);
            SubItems.Add(SysInfo.EventLog.Records[i].Source);
            SubItems.Add(SysInfo.EventLog.Records[i].Category);
            SubItems.Add(IntToStr(SysInfo.EventLog.Records[i].EventID));
            SubItems.Add(SysInfo.EventLog.Records[i].Username);
            SubItems.Add(SysInfo.EventLog.Records[i].Computer);
            SubItems.Add(SysInfo.EventLog.Records[i].Description);
            ImageIndex:=Integer(SysInfo.EventLog.Records[i].EventType);
          end;
      finally
        EndUpdate;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tdlg_msi_Overview.cbCPUChange(Sender: TObject);
begin
  with SysInfo.CPU do begin
    CPUIndex:=PInteger(cbCPU.Items.Objects[cbCPU.ItemIndex])^;
    if LiveData then
      RefreshData
    else
      LoadFromSIF(SysInfo.CPU);
    lCPU.caption:=Format('%s %s - %d MHz',[cVendorNames[Vendor].Prefix,CPUName,Frequency]);
    case Architecture of
      PROCESSOR_ARCHITECTURE_AMD64: lArch.Caption:='Architecture: x64 (AMD or Intel)';
      PROCESSOR_ARCHITECTURE_IA32_ON_WIN64: lArch.Caption:='Architecture: WOW64';
      PROCESSOR_ARCHITECTURE_IA64: lArch.Caption:='Architecture: Intel Itanium Processor Family (IPF)';
      PROCESSOR_ARCHITECTURE_INTEL: lArch.Caption:='Architecture: x86';
      else lArch.Caption:=Format('Architecture: %d',[Architecture]);
    end;
    lCPP.Caption:=Format('Core per package: %d',[CorePerPackage]);
    lLPC.Caption:=Format('Logical per core: %d',[LogicalPerCore]);
    lCC.Caption:=Format('Core count: %d',[CoreCount]);
    lTHC.Caption:=Format('Thread count: %d',[ThreadCount]);
    lMarket.Caption:=Format('Marketing name: %s',[MarketingName]);
    lGeneric.Caption:=Format('Generic name: %s',[GenericName]);
    lCodename.Visible:=Codename<>'';
    lCodename.Caption:=Format('Code Name: %s',[CodeName]);
    lRevision.Visible:=Revision<>'';
    lRevision.Caption:=Format('Revision: %s',[Revision]);
    lTech.Visible:=Technology<>'';
    lTech.Caption:=Format('Technology: %s',[Technology]);
    lFreq.Caption:=Format('Frequency: %d MHz',[Frequency]);
    lVendor.Caption:=Format('Vendor: %s',[cVendorNames[Vendor].Name]);
    lSerial.Caption:=Format('Serial Number: %s',[SerialNumber]);

    CacheBox.Lines.Clear;
    with Cache.Level1.Code do
      if Size>0 then begin
        if Associativity<>caNone then
          CacheBox.Lines.Add(Format('Level1 Code: %d x %d KB, %s, %d entries',[SharedWays,Size,cAssociativityDescription[Associativity],LineSize]))
        else
          CacheBox.Lines.Add(Format('%d x %s',[SharedWays,Descriptor]));
      end;
    with Cache.Level1.Data do
      if Size>0 then begin
        if Associativity<>caNone then
          CacheBox.Lines.Add(Format('Level1 Data: %d x %d KB, %s, %d entries',[SharedWays,Size,cAssociativityDescription[Associativity],LineSize]))
        else
          CacheBox.Lines.Add(Format('%d x %s',[SharedWays,Descriptor]));
      end;
    with Cache.Level1.Unified do
      if Size>0 then begin
        if Associativity<>caNone then
          CacheBox.Lines.Add(Format('Level1 Unified: %d x %d KB, %s, %d entries',[SharedWays,Size,cAssociativityDescription[Associativity],LineSize]))
        else
          CacheBox.Lines.Add(Format('%d x %s',[SharedWays,Descriptor]));
      end;
    CacheBox.Lines.Add('');
    with Cache.Level2 do
      if Size>0 then begin
        if Associativity<>caNone then
          CacheBox.Lines.Add(Format('Level2: %d x %d K, %s, %d entries',[SharedWays,Size,cAssociativityDescription[Associativity],LineSize]))
        else
          CacheBox.Lines.Add(Format('%d x %s',[SharedWays,Descriptor]));
      end;
   CacheBox.Lines.Add('');
    with Cache.Level3 do
      if Size>0 then begin
        if Associativity<>caNone then
          CacheBox.Lines.Add(Format('Level3: %d x %d K, %s, %d entries',[SharedWays,Size,cAssociativityDescription[Associativity],LineSize]))
        else
          CacheBox.Lines.Add(Format('%d x %s',[SharedWays,Descriptor]));
      end;
    CacheBox.Lines.Add('');
    with Cache.Trace do
      if Size>0 then
        CacheBox.Lines.Add(Format('Trace: %d K, %s, %d entries',[SharedWays*Size,cAssociativityDescription[Associativity],LineSize]));
    tcChange(nil);
  end;
end;

procedure Tdlg_msi_Overview.GetCPUInfo;
var
  i: Integer;
  pi: PInteger;
  s: string;
begin
  for i:=0 to cbCPU.Items.Count-1 do
    if Assigned(cbCPU.Items.Objects[i]) then
      dispose(PInteger(cbCPU.Items.Objects[i]));
  cbCPU.Clear;
  with SysInfo.CPU do begin
    for i:=0 to CPUPhysicalCount-1 do begin
      CPUIndex:=i;
      if LiveData then
        RefreshData
      else
        LoadFromSIF(SysInfo.CPU);
      s:=Format('Processor #%d',[PhysicalID]);
      if cbCPU.Items.IndexOf(s)=-1 then begin
        new(pi);
        pi^:=i;
        cbCPU.Items.AddObject(s,@pi^);
      end;
    end;
    if cbCPU.Items.Count<CPUPhysicalCount then begin
      new(pi);
      pi^:=i-1;
      cbCPU.Items.AddObject(s,@pi^);
    end;
    cbCPU.ItemIndex:=0;
    cbCPUChange(nil);
  end;
end;

procedure Tdlg_msi_Overview.tcChange(Sender: TObject);
var
  i: Integer;
begin
  clbFS.Items.Clear;
  with SysInfo.CPU.Features do
    case tc.TabIndex of
      0: for i:=0 to Standard1.Count-1 do begin
           clbFS.Items.Add(Format('%s - %s',[Standard1.Features[i].Mnemonic,Standard1.Features[i].Name]));
           clbFS.Checked[clbFS.Items.Count-1]:=Standard1.Features[i].Available;
         end;
      1: for i:=0 to Standard2.Count-1 do begin
           clbFS.Items.Add(Format('%s - %s',[Standard2.Features[i].Mnemonic,Standard2.Features[i].Name]));
           clbFS.Checked[clbFS.Items.Count-1]:=Standard2.Features[i].Available;
         end;
      2: for i:=0 to Extended1.Count-1 do begin
           clbFS.Items.Add(Format('%s - %s',[Extended1.Features[i].Mnemonic,Extended1.Features[i].Name]));
           clbFS.Checked[clbFS.Items.Count-1]:=Extended1.Features[i].Available;
         end;
      3: for i:=0 to Extended2.Count-1 do begin
           clbFS.Items.Add(Format('%s - %s',[Extended2.Features[i].Mnemonic,Extended2.Features[i].Name]));
           clbFS.Checked[clbFS.Items.Count-1]:=Extended2.Features[i].Available;
         end;
      4: for i:=0 to PowerManagement.Count-1 do begin
           clbFS.Items.Add(Format('%s - %s',[PowerManagement.Features[i].Mnemonic,PowerManagement.Features[i].Name]));
           clbFS.Checked[clbFS.Items.Count-1]:=PowerManagement.Features[i].Available;
         end;
    end;
end;

procedure Tdlg_msi_Overview.StorageTreeChange(Sender: TObject;
  Node: TTreeNode);
var
  s: string;
begin
  lvStorage.Items.BeginUpdate;
  lvStorage.Columns.BeginUpdate;
  try
    lvStorage.Items.Clear;
    if Assigned(Node) and Assigned(Node.Data) then
      if Node.Level=0 then with SysInfo.Storage.Physical[PInteger(Node.Data)^] do begin
        with lvStorage.Items.Add do begin
          Caption:='Serial number';
          SubItems.Add(SerialNumber);
        end;
        with lvStorage.Items.Add do begin
          Caption:='Revision';
          SubItems.Add(Revision);
        end;

        with lvStorage.Items.Add do begin
          Caption:='Model';
          SubItems.Add(Model);
        end;

        with lvStorage.Items.Add do begin
          Caption:='Bus Type';
          SubItems.Add(GetStorageBusTypeStr(BusType));
        end;
        s:='';
        case DeviceType of
          FILE_DEVICE_CD_ROM: s:='CD-ROM';
          FILE_DEVICE_DVD: s:='DVD';
          FILE_DEVICE_MASS_STORAGE: s:='Mass Storage';
          FILE_DEVICE_TAPE: s:='Tape';
          FILE_DEVICE_DISK: begin
            s:='Drive';
            if SSD then
              s:='Solid State Drive';
          end;
        end;
        if s<>'' then
          with lvStorage.Items.Add do begin
            Caption:='Device Type';
            SubItems.Add(s);
          end;
        with lvStorage.Items.Add do begin
          Caption:='Media Type';
          SubItems.Add(GetDeviceMediaTypeStr(MediaType));
        end;
        if (DeviceType=FILE_DEVICE_CD_ROM) or (DeviceType=FILE_DEVICE_DVD) then begin
          s:='';
          if Read_CDRW then
            s:=s+'CD-R,';
          if Read_CDR then
            s:=s+'CD-RW,';
          if Read_DVDROM then
            s:=s+'DVD-ROM,';
          if Read_DVDR then
            s:=s+'DVD-R,';
          if Read_DVDRAM then
            s:=s+'DVD-RAM,';
          SetLength(s,Length(s)-1);
          with lvStorage.Items.Add do begin
            Caption:='Read Caps';
            SubItems.Add(s);
          end;
          s:='';
          if Write_CDRW then
            s:=s+'CD-R,';
          if Write_CDR then
            s:=s+'CD-RW,';
          if Write_DVDR then
            s:=s+'DVD-R,';
          if Write_DVDRAM then
            s:=s+'DVD-RAM,';
          SetLength(s,Length(s)-1);
          with lvStorage.Items.Add do begin
            Caption:='Write Caps';
            SubItems.Add(s);
          end;
        end;
        if HaId>=0 then
          with lvStorage.Items.Add do begin
            Caption:='SCSI adapter';
            SubItems.Add(IntToStr(HaId));
          end;
        if PathId>=0 then
          with lvStorage.Items.Add do begin
            Caption:='Bus';
            SubItems.Add(IntToStr(PathId));
          end;
        if Target>=0 then
          with lvStorage.Items.Add do begin
            Caption:='Target device';
            SubItems.Add(IntToStr(Target));
          end;
        if Lun>=0 then
          with lvStorage.Items.Add do begin
            Caption:='Logical unit number';
            SubItems.Add(IntToStr(Lun));
          end;

        if Temperature>0 then
          with lvStorage.Items.Add do begin
            Caption:='Temperature';
            SubItems.Add(Format('%d%sC',[Temperature,{$IFDEF UNICODE}#$00B0{$ELSE}'°'{$ENDIF}]));
          end;
        if Size>0 then begin
          with lvStorage.Items.Add do begin
            Caption:='Capacity';
            SubItems.Add(Format('%d MB',[Size shr 20]));
          end;
          with lvStorage.Items.Add do begin
            Caption:='Cyls';
            SubItems.Add(Format('%d',[Geometry.Cylinders.QuadPart]));
          end;
          with lvStorage.Items.Add do begin
            Caption:='Heads';
            SubItems.Add(Format('%d',[Geometry.TracksPerCylinder]));
          end;
          with lvStorage.Items.Add do begin
            Caption:='Sectors per track';
            SubItems.Add(Format('%d',[Geometry.SectorsPerTrack]));
          end;
          with lvStorage.Items.Add do begin
            Caption:='Bytes per sector';
            SubItems.Add(Format('%d',[Geometry.BytesPerSector]));
          end;
          if IdentifyDeviceData.Max48BitLBA[0]=0 then
            with lvStorage.Items.Add do begin
              Caption:='Physical sectors';
              if (Geometry.BytesPerSector>0) then
                SubItems.Add(Format('%d',[LengthInBytes div Geometry.BytesPerSector]))
              else
                SubItems.Add(Format('%d',[Geometry.Cylinders.QuadPart*Geometry.TracksPerCylinder*Geometry.SectorsPerTrack]));
            end
          else begin
            with lvStorage.Items.Add do begin
              Caption:='Number of sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.CurrentSectorCapacity]));
            end;
            with lvStorage.Items.Add do begin
              Caption:='Total 32-bit LBA sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.UserAddressableSectors]));
            end;
            with lvStorage.Items.Add do begin
              Caption:='Total 48-bit LBA sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.Max48BitLBA[0]]));
            end;
          end;
        end;
        if IdentifyDeviceData.MajorRevision>0 then
          with lvStorage.Items.Add do begin
            Caption:='ATA Major version';
            SubItems.Add(GetATAMajorVersion(IdentifyDeviceData.MajorRevision));
          end;
        if IdentifyDeviceData.MinorRevision>0 then
          with lvStorage.Items.Add do begin
            Caption:='ATA Minor version';
            SubItems.Add(GetATAMinorVersion(IdentifyDeviceData.MinorRevision));
          end;
        if IdentifyDeviceData.ReservedWord220[2]>0 then
          with lvStorage.Items.Add do begin
            Caption:='ATA Transport version';
            SubItems.Add(GetATATransportVersion(IdentifyDeviceData.ReservedWord220[2]));
          end;
        if ECCCode<>0 then
          with lvStorage.Items.Add do begin
            Caption:='ECC';
            SubItems.Add(Format('%d',[ECCCode]));
          end;
        if CtlBufSize<>0 then
          with lvStorage.Items.Add do begin
            Caption:='Cache Buffer size';
            SubItems.Add(Format('%d KB',[CtlBufSize shr 10]));
          end;
        with lvStorage.Items.Add do begin
          Caption:='S.M.A.R.T.';
          if SMARTSupport then begin
            if SMARTActive then
              SubItems.Add('Supported and active')
            else
              SubItems.Add('Supported and NOT active');
          end else
            SubItems.Add('NOT supported');
        end;
      end else begin
        SysInfo.Disk.Drive:=SysInfo.Storage.Logical[PInteger(Node.Data)^].Drive+':';
        if not SysInfo.Disk.Livedata then
           LoadFromSIF(SysInfo.Disk);
        with lvStorage.Items.Add do begin
          Caption:='Volume label';
          SubItems.Add(SysInfo.Disk.VolumeLabel);
        end;
        with lvStorage.Items.Add do begin
          Caption:='Serial number';
          SubItems.Add(SysInfo.Disk.SerialNumber);
        end;
        with lvStorage.Items.Add do begin
          Caption:='Capacity';
          SubItems.Add(Format('%d MB',[SysInfo.Disk.Capacity shr 20]));
        end;
        with lvStorage.Items.Add do begin
          Caption:='Free space';
          SubItems.Add(Format('%d MB',[SysInfo.Disk.FreeSpace shr 20]));
        end;
        with lvStorage.Items.Add do begin
          Caption:='File system';
          SubItems.Add(Sysinfo.Disk.FileSystem);
        end;
        with SysInfo.Storage.Logical[PInteger(Node.Data)^] do
          if LayoutIndex>-1 then begin
            with lvStorage.Items.Add do begin
              Caption:='First sector';
              SubItems.Add(IntToStr(Layout[LayoutIndex].StartingOffset.QuadPart div Geometry.BytesPerSector));
            end;
            with lvStorage.Items.Add do begin
              Caption:='Last sector';
              SubItems.Add(IntToStr((Layout[LayoutIndex].StartingOffset.QuadPart+Layout[LayoutIndex].Length.QuadPart-1) div Geometry.BytesPerSector));
            end;
            with lvStorage.Items.Add do begin
              Caption:='Cluster size';
              SubItems.Add(Format('%d B',[ClusterSize]));
            end;
            with lvStorage.Items.Add do begin
              Caption:='Total sectors';
              SubItems.Add(IntToStr(Layout[LayoutIndex].Length.QuadPart div Geometry.BytesPerSector));
            end;
          end;
      end;
  finally
    lvStorage.Items.EndUpdate;
    lvStorage.Columns.EndUpdate;
  end;
end;

end.

