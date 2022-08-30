unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, Menus, MSI_SMBIOS, ComCtrls, AppEvnts, MSI_DMA, StdCtrls,
  MSI_Common;

type
  TLastSearchMethod = (smSequence, smText);

  TappRBE = class(TForm)
    MainMenu: TMainMenu;
    GridPanel: TPanel;
    sghex: TStringGrid;
    sgchar: TStringGrid;
    BottomPanel: TPanel;
    Panel29: TPanel;
    lvTables: TListView;
    stPanel: TPanel;
    Panel5: TPanel;
    Panel4: TPanel;
    sgVals: TStringGrid;
    File1: TMenuItem;
    mmSave: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    GotoAddress1: TMenuItem;
    mmOpen: TMenuItem;
    ReloadLocalMemory1: TMenuItem;
    N3: TMenuItem;
    Search1: TMenuItem;
    N2: TMenuItem;
    mmFindText: TMenuItem;
    Findnext1: TMenuItem;
    About1: TMenuItem;
    fd: TFindDialog;
    sb: TStatusBar;
    N4: TMenuItem;
    Details1: TMenuItem;
    N5: TMenuItem;
    mmFindSeq: TMenuItem;
    N6: TMenuItem;
    mmXML: TMenuItem;
    od: TOpenDialog;
    ed: TSaveDialog;
    sd: TSaveDialog;
    mmSaveRawMemory: TMenuItem;
    rd: TSaveDialog;
    procedure sghexSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sgcharSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lvTablesDblClick(Sender: TObject);
    procedure sgValsDblClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure GotoAddress1Click(Sender: TObject);
    procedure ReloadLocalMemory1Click(Sender: TObject);
    procedure sgEnter(Sender: TObject);
    procedure sgExit(Sender: TObject);
    procedure lvTablesEnter(Sender: TObject);
    procedure lvTablesExit(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure mmFindTextClick(Sender: TObject);
    procedure fdFind(Sender: TObject);
    procedure Findnext1Click(Sender: TObject);
    procedure fdShow(Sender: TObject);
    procedure sghexTopLeftChanged(Sender: TObject);
    procedure sgcharTopLeftChanged(Sender: TObject);
    procedure sghexKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgcharKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Details1Click(Sender: TObject);
    procedure mmFindSeqClick(Sender: TObject);
    procedure mmXMLClick(Sender: TObject);
    procedure cmSaveDump(Sender: TObject);
    procedure mmOpenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmSaveRawMemoryClick(Sender: TObject);
  private
    SMBIOS: TMiTeC_SMBIOS;
    CurrentAddress, StartAddress, Size: DWORD;
    LastSearchmethod: TLastSearchmethod;
    procedure FindSeq;
    procedure ReadData(AWidth: Byte = 16);
    procedure Compute(cp: DWORD);
    procedure GotoAddress(a: DWORD);
  public
    procedure OpenFile(AFilename: string);
    procedure OpenLocal;
    procedure SaveSMBIOS;
    procedure SaveRaw;
  end;

var
  appRBE: TappRBE;
const
  extDMP = '.dmp';
  extRAW = '.bin';
  extSMBIOS = '.smbios';

implementation

uses ShellAPI, FileCtrl, MiTeC_Dialogs, Details, MiTeC_Routines, MiTeC_SIF, Math;

var
  Sequence: string;

{$R *.DFM}

procedure TappRBE.Compute;
const
  vs = 6;
var
  ix :array[0..7] of int64;
  i :integer;
  i64: Int64;
  f64: Double absolute i64;
begin
  sgVals.Cells[1,vs-1]:=IntToHex(cp,8);

  for i:=0 to high(ix) do
    if cp+i<=StartAddress+Size then
      ix[i]:=SMBIOS.SMBIOS_DMA.ByteValue[cp+i]
    else
      ix[i]:=0;

  sgVals.Cells[1,vs+0]:=Format('%d (%x)',[shortint(ix[0]),shortint(ix[0])]);
  sgVals.Cells[1,vs+2]:=Format('%d (%x)',[smallint(ix[0]+ix[1]*256),smallint(ix[0]+ix[1]*256)]);
  sgVals.Cells[1,vs+4]:=Format('%d (%x)',[longint(ix[0]+ix[1]*256)+(ix[2]+ix[3]*256)*65536,longint(ix[0]+ix[1]*256)+(ix[2]+ix[3]*256)*65536]);
  i64:=int64(ix[0]+ix[1]*256)+(ix[2]+ix[3]*256)*65536+((ix[4]+ix[5]*256)+(ix[6]+ix[7]*256)*65536)*4294967296;
  sgVals.Cells[1,vs+6]:=IntToStr(i64);

  sgVals.Cells[1,vs+1]:=Format('%d (%x)',[byte(ix[0]),byte(ix[0])]);
  sgVals.Cells[1,vs+3]:=Format('%d (%x)',[word(ix[0]+ix[1]*256),word(ix[0]+ix[1]*256)]);
  sgVals.Cells[1,vs+5]:=Format('%d (%x)',[longword(ix[0]+ix[1]*256)+(ix[2]+ix[3]*256)*65536,longword(ix[0]+ix[1]*256)+(ix[2]+ix[3]*256)*65536]);
  sgVals.Cells[1,vs+7]:=FloatToStr(f64);
end;

procedure TappRBE.sghexSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if (Sender=nil) or twincontrol(sender).Focused then begin
    if not(goRangeSelect in sgHex.Options) then begin
      sgchar.row:=arow;
      sgchar.col:=acol-1;
    end;
    CurrentAddress:=(sghex.ColCount-1)*(ARow-1)+(ACol-1)+StartAddress;
    Compute(CurrentAddress);
  end;
end;

procedure TappRBE.sgcharSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if (sender=nil) or twincontrol(sender).Focused then begin
    if not(goRangeSelect in sgHex.Options) then begin
      sghex.row:=arow;
      sghex.col:=acol+1;
    end;
    CurrentAddress:=(sghex.ColCount-1)*(ARow-1)+(ACol)+StartAddress;
    Compute(CurrentAddress);
  end;
end;

procedure TappRBE.ReadData;
var
  i,j: Integer;
  b: Byte;
begin
  sghex.Hide;
  sgchar.Hide;
  Update;
  try
  sghex.row:=1;
  sghex.rowcount:=2;
  sghex.colwidths[0]:=65;
  sghex.colcount:=AWidth+1;
  sgchar.colcount:=sghex.colcount-1;
  sgchar.rowcount:=sghex.RowCount;
  for i:=1 to AWidth do begin
    sghex.cells[i,0]:=inttohex(i-1,2);
    sgchar.cells[i-1,0]:=inttohex(i-1,1);
  end;
  j:=1;
  for i:=StartAddress to StartAddress+Size do begin
    if j>AWidth then begin
      sghex.cells[0,sghex.row]:=IntToHex(i-AWidth,8);
      sghex.rowcount:=sghex.rowcount+1;
      sgchar.rowcount:=sghex.rowcount;
      sghex.row:=sghex.row+1;
      sghex.cells[0,sghex.row]:=IntToHex(i,8);
      j:=1;
    end;
    b:=SMBIOS.SMBIOS_DMA.ByteValue[i];
    sghex.cells[j,sghex.row]:=Format('%2.2x',[b]);
    if b in [0..31, 127..255] then
      sgchar.cells[j-1,sghex.row]:='.'
    else
      sgchar.cells[j-1,sghex.row]:=Chr(b);
    inc(j);
  end;

  sgVals.Cells[1,0]:=Format('%d.%d',[SMBIOS.MajorVersion,SMBIOS.MinorVersion]);
  sgVals.Cells[1,1]:=Format('%d.%d',[SMBIOS.RevisionMajor,SMBIOS.RevisionMinor]);
  sgVals.Cells[1,2]:=Format('%8.8x',[SMBIOS.SMBIOSAddress]);
  sgVals.Cells[1,3]:=Format('%8.8x',[SMBIOS.StructStart]);
  sgVals.Cells[1,4]:=Format('%d',[SMBIOS.StructLength]);

  stPanel.Caption:=Format('  Structure Tables (%d from %d found)',[Length(SMBIOS.StructTables),SMBIOS.StructCount]);
  lvTables.Items.Clear;
  for i:=0 to High(SMBIOS.StructTables) do
    with lvTables.Items.Add do begin
      Caption:=Format('Type %d: %s',[SMBIOS.StructTables[i].Indicator,SMBIOS.StructTables[i].Name]);
      SubItems.Add(Format('%d',[SMBIOS.StructTables[i].Length]));
      SubItems.Add(Format('%4.4x',[SMBIOS.StructTables[i].Handle]));
      SubItems.Add(Format('%8.8x',[SMBIOS.StructTables[i].Address]));
      ImageIndex:=-1;
    end;
  finally
    sghex.Show;
    sgchar.Show;
    Caption:=Format('%s - [%s %s %s]',[Application.Title,Trim(SMBIOS.SystemModel),Trim(SMBIOS.BIOSVendor),Trim(SMBIOS.BIOSVersion)]);
    sb.Panels[0].Text:=Format('BIOS size: %d K',[SMBIOS.BIOSSize]);
    sb.Panels[1].Text:=Format('BIOS date: %s',[SMBIOS.BIOSDate]);
  end;
  sghex.Col:=1;
  sghex.Row:=1;
end;

procedure TappRBE.FormCreate(Sender: TObject);
begin
  LastSearchmethod:=smText;
  CurrentAddress:=RomBiosDumpBase;
  sgVals.ColWidths[0]:=90;
  sgVals.ColWidths[1]:=135;
  sgVals.Cells[0,0]:='SMBIOS Version';
  sgVals.Cells[0,1]:='SMBIOS Revision';
  sgVals.Cells[0,2]:='SMBIOS Address';
  sgVals.Cells[0,3]:='Structure Address';
  sgVals.Cells[0,4]:='Structure Length';
  sgVals.Cells[0,5]:='Cursor Address';
  sgVals.Cells[0,6]:='Signed 8-bit';
  sgVals.Cells[0,7]:='Unsigned 8-bit';
  sgVals.Cells[0,8]:='Signed 16-bit';
  sgVals.Cells[0,9]:='Unsigned 16-bit';
  sgVals.Cells[0,10]:='Signed 32-bit';
  sgVals.Cells[0,11]:='Unsigned 32-bit';
  sgVals.Cells[0,12]:='Signed 64-bit';
  sgVals.Cells[0,13]:='Float 64-bit';
  SMBIOS:=TMiTeC_SMBIOS.Create(Self);
end;

procedure TappRBE.FormShow(Sender: TObject);
begin
  if (ParamCount>0) and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1))
  else begin
    OpenLocal;
    if FindCmdLineSwitch('S') then
      SaveSMBIOS;
  end;
end;

procedure TappRBE.lvTablesDblClick(Sender: TObject);
begin
  if Assigned(lvTables.Selected) then begin
    GotoAddress(StrToInt('$'+lvTables.Selected.SubItems[2]));
    //ReadData;
  end;
end;

procedure TappRBE.GotoAddress(a: DWORD);
var
  b: Boolean;
begin
  try
    a:=a-StartAddress;
    sgHex.Row:=a div (sghex.ColCount-1)+1;
    sghex.Col:=a mod (sghex.ColCount-1)+1;
    sghexSelectCell(nil,sghex.Col,sgHex.Row,b);
    sghex.SetFocus;
  except
    MessageDlg(Format('Invalid address [%x]',[a+StartAddress]),mtError,[mbOK],0);
  end;
end;

procedure TappRBE.sgValsDblClick(Sender: TObject);
begin
  if sgVals.Row=1 then
    GotoAddress(SMBIOS.SMBIOSAddress)
  else
    if sgVals.Row=2 then
      GotoAddress(SMBIOS.StructStart)
    else
      if sgVals.Row=10 then
        GotoAddress(SMBIOS.SMBIOS_DMA.DWORDValue[CurrentAddress])
end;

procedure TappRBE.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TappRBE.GotoAddress1Click(Sender: TObject);
var
  s: string;
begin
  if InputQuery('Go to Adress...','Hex address',s) then
    GotoAddress(StrToInt('$'+s));
end;

procedure TappRBE.ReloadLocalMemory1Click(Sender: TObject);
begin
  OpenLocal;
end;

procedure TappRBE.SaveRaw;
var
  s: string;
begin
  s:=Format('%s %s %s %s - SMBIOS_%d_%d_%d_%d',[Trim(SMBIOS.SystemModel),Trim(SMBIOS.BIOSVendor),Trim(SMBIOS.BIOSVersion),Trim(SMBIOS.BIOSDate),SMBIOS.MajorVersion,SMBIOS.MinorVersion,SMBIOS.RevisionMajor,SMBIOS.RevisionMinor]);
  s:=StringReplace(s,',',' ',[rfReplaceAll]);
  s:=StringReplace(s,'/','-',[rfReplaceAll]);
  if Trim(s)='' then
    s:='unknown';
  s:=s+extRaw;
  SMBIOS.RAW_DMA.SaveToFile(s,nil);
end;

procedure TappRBE.SaveSMBIOS;
var
  s: string;
  b: TBytes;
begin
  s:=Format('%s %s %s %s - SMBIOS_%d_%d_%d_%d',[Trim(SMBIOS.SystemModel),Trim(SMBIOS.BIOSVendor),Trim(SMBIOS.BIOSVersion),Trim(SMBIOS.BIOSDate),SMBIOS.MajorVersion,SMBIOS.MinorVersion,SMBIOS.RevisionMajor,SMBIOS.RevisionMinor]);
  s:=StringReplace(s,',',' ',[rfReplaceAll]);
  s:=StringReplace(s,'/','-',[rfReplaceAll]);
  if Trim(s)='' then
    s:='unknown';
  s:=s+extSMBIOS;
  b:=TBytes.Create($53,$4D,$42,$49,$4F,$53,0,SMBIOS.MajorVersion,SMBIOS.MinorVersion,StrToInt(Format('$%d%d',[SMBIOS.RevisionMajor,SMBIOS.RevisionMinor])),Lo(SMBIOS.StructCount),Hi(SMBIOS.StructCount),0,0,0,0);
  SMBIOS.SMBIOS_DMA.SaveToFile(s,b);
end;

procedure TappRBE.sgEnter(Sender: TObject);
begin
  TStringGrid(Sender).Color:=clInfoBk;
end;

procedure TappRBE.sgExit(Sender: TObject);
begin
  TStringGrid(Sender).Color:=clWhite;
end;

procedure TappRBE.lvTablesEnter(Sender: TObject);
begin
  TListView(Sender).Color:=clInfoBk;
end;

procedure TappRBE.lvTablesExit(Sender: TObject);
begin
  TListView(Sender).Color:=clWhite;
end;

procedure TappRBE.About1Click(Sender: TObject);
begin
  ShellAbout(Handle,PChar(Application.Title+' '+ModuleInfo.FileVersion),PChar(ModuleInfo.Copyright),Application.Icon.Handle);
end;

procedure TappRBE.mmFindTextClick(Sender: TObject);
begin
  fd.Execute;
end;

procedure TappRBE.fdFind(Sender: TObject);
var
  b,i: DWORD;
  p: integer;
  s,f: string;
  c: Char;
begin
  LastSearchmethod:=smText;
  b:=CurrentAddress+1;
  f:=fd.FindText;
  if not(frMatchCase in fd.Options) then
    f:=Uppercase(f);
  p:=-1;
  for i:=b to StartAddress+Size do begin
    c:=Chr(SMBIOS.SMBIOS_DMA.ByteValue[i]);
    if not(frMatchCase in fd.Options) and (c in ['a'..'z']) then
      c:=UpCase(c);
    if (c=f[1]) then begin
      s:=SMBIOS.SMBIOS_DMA.ArrayValue[i,Length(f)];
      if not(frMatchCase in fd.Options) then
        s:=UpperCase(s);
      p:=Pos(f,s);
      if p>0 then begin
        p:=p+i-1;
        Break;
      end;
    end;
  end;
  if p>=StartAddress then
    GotoAddress(p)
  else
    MessageDlg('Text not found.',mtInformation,[mbOK],0);
end;

procedure TappRBE.Findnext1Click(Sender: TObject);
begin
  if LastSearchMethod=smSequence then
    FindSeq
  else begin
    if fd.FindText<>'' then
      fdFind(nil)
    else
      fd.Execute;
  end;
end;

procedure TappRBE.fdShow(Sender: TObject);
begin
  fd.FindText:='';
end;

procedure TappRBE.sghexTopLeftChanged(Sender: TObject);
begin
  if sgHex.Focused then
    sgChar.TopRow:=sgHex.TopRow;
end;

procedure TappRBE.sgcharTopLeftChanged(Sender: TObject);
begin
  if sgChar.Focused then
    sgHex.TopRow:=sgChar.TopRow;
end;

procedure TappRBE.sghexKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RIGHT:
      if (sgHex.Col=sgHex.ColCount-1) and (sgHex.Row<sgHex.RowCount-1) then begin
        sgHex.Row:=sgHex.Row+1;
        sgHex.Col:=1;
        Key:=0;
      end;
    VK_LEFT:
      if (sgHex.Col=1) and (sgHex.Row>1) then begin
        sgHex.Row:=sgHex.Row-1;
        sgHex.Col:=sgHex.ColCount-1;
        Key:=0;
      end;
  end;
end ;

procedure TappRBE.sgcharKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RIGHT:
      if (sgChar.Col=sgChar.ColCount-1) and (sgChar.Row<sgChar.RowCount-1) then begin
        sgChar.Row:=sgChar.Row+1;
        sgChar.Col:=0;
        Key:=0;
      end;
    VK_LEFT:
      if (sgChar.Col=0) and (sgChar.Row>1) then begin
        sgChar.Row:=sgChar.Row-1;
        sgChar.Col:=sgChar.ColCount-1;
        Key:=0;
      end;
  end;
end;

procedure TappRBE.Details1Click(Sender: TObject);
var
  i: Integer;
begin
  with TwndDetails.Create(Self) do begin
    with SMBIOS do begin
      eSMVer.Text:=Format('%d.%d. Rev. %d.%d',[MajorVersion,MinorVersion,RevisionMajor,RevisionMinor]);
      eSMTables.Text:=Format('%d',[Length(StructTables)]);

      eBIOSVendor.Text:=BIOSVendor;
      eBIOSVer.Text:=BIOSVersion;
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

      eCHMod.Text:=ChassisTypes[ChassisModel];
      eCHMan.Text:=ChassisManufacturer;
      eCHVer.Text:=ChassisVersion;
      eCHSer.Text:=ChassisSerial;
      eCHAT.Text:=ChassisAssetTag;

      eMCI.Text:=Format('%s / %s',[InterleaveSupports[MemCtrlCurrentInterleave],
                                   InterleaveSupports[MemCtrlSupportedInterleave]]);
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
          SubItems.Add(Upgrades[Processor[i].Upgrade]);
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
          SubItems.Add(CacheTypes[Cache[i].Typ]);
          SubItems.Add(CacheAssociativities[Cache[i].Associativity]);
          SubItems.Add(SRAMTypes[Cache[i].SRAMType]);
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
          SubItems.Add(MemoryDeviceTypes[MemoryDevice[i].Device]);
          SubItems.Add(GetMemoryTypeDetailsStr(MemoryDevice[i].TypeDetails));
          SubItems.Add(MemoryFormFactors[MemoryDevice[i].FormFactor]);
          SubItems.Add(Format('%d MB',[MemoryDevice[i].Size]));
          SubItems.Add(Format('%d MHz',[MemoryDevice[i].Speed]));
          SubItems.Add(Format('%d MHz',[MemoryDevice[i].MaxSpeed]));
          SubItems.Add(Format('%d b',[MemoryDevice[i].TotalWidth]));
          SubItems.Add(Format('%d b',[MemoryDevice[i].DataWidth]));
          SubItems.Add(MemoryDevice[i].Manufacturer);
          SubItems.Add(MemoryDevice[i].SerialNumber);
          SubItems.Add(MemoryDevice[i].AssetTag);
          SubItems.Add(MemoryDevice[i].PartNumber);
          SubItems.Add(MemoryTechnologies[MemoryDevice[i].MemoryTechnology]);
          ImageIndex:=-1;
        end;

      lvPort.Items.Clear;
      for i:=0 to PortCount-1 do
        with lvPort.Items.Add do begin
          Caption:=PortTypes[Port[i].Typ];
          SubItems.Add(Port[i].InternalDesignator);
          SubItems.Add(ConnectorTypes[Port[i].InternalConnector]);
          SubItems.Add(Port[i].ExternalDesignator);
          SubItems.Add(ConnectorTypes[Port[i].ExternalConnector]);
          ImageIndex:=-1;
        end;

      lvSlot.Items.Clear;
      for i:=0 to SystemSlotCount-1 do
        with lvSlot.Items.Add do begin
          Caption:=SlotTypes[SystemSlot[i].Typ];
          SubItems.Add(DataBusTypes[SystemSlot[i].DataBus]);
          SubItems.Add(SlotUsages[SystemSlot[i].Usage]);
          SubItems.Add(SlotLengths[SystemSlot[i].Length]);
          SubItems.Add(IntToStr(SystemSlot[i].BusNumber));
          SubItems.Add(IntToStr(SystemSlot[i].DevNumber));
          SubItems.Add(IntToStr(SystemSlot[i].FuncNumber));
          ImageIndex:=-1;
        end;

      lvOBD.Items.Clear;
      for i:=0 to OnBoardDeviceCount-1 do
        with lvOBD.Items.Add do begin
          Caption:=OnBoardDevice[i].DeviceName;
          SubItems.Add(OnBoardDeviceTypes[OnBoardDevice[i].Typ]);
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
    ShowModal;
    Free;
  end;
end;

procedure TappRBE.mmFindSeqClick(Sender: TObject);
begin
  if InputQuery('Find Sequence...','Sequence as hex string',Sequence) then
    FindSeq;
end;

procedure TappRBE.FindSeq;
var
  a: DWORD;
begin
  LastSearchmethod:=smSequence;
  a:=SMBIOS.SMBIOS_DMA.FindSequence(CurrentAddress+1,Sequence);
  if a>0 then
    Gotoaddress(a)
  else
    MessageDlg('Sequence not found.',mtInformation,[mbOK],0);
end;

procedure TappRBE.mmXMLClick(Sender: TObject);
var
  h: Boolean;
begin
  h:=True;
  ed.Filename:=Format('%s %s %s %s',[Trim(SMBIOS.SystemModel),Trim(SMBIOS.BIOSVendor),Trim(SMBIOS.BIOSVersion),Trim(SMBIOS.BIOSDate)]);
  ed.Filename:=StringReplace(ed.Filename,',',' ',[rfReplaceAll]);
  ed.Filename:=StringReplace(ed.Filename,'/','-',[rfReplaceAll]);
  if Trim(ed.Filename)='' then
    ed.Filename:='unknown';
  ed.Filename:=ed.Filename+cSIFExt;
  if ed.Execute then
    SMBIOS.SaveToStorage(ed.FileName,h);
end;

procedure TappRBE.OpenFile(AFilename: string);
begin
  mmSaveRawMemory.Enabled:=False;
  mmSave.Enabled:=False;
  SMBIOS.ReadLocalMemory:=False;
  if SameText(ExtractFileExt(AFilename),'.smbios') then begin
    SMBIOS.LoadSMBIOSFromFile(AFilename);
    StartAddress:=SMBIOS.SMBIOS_DMA.StartAddress;
    Size:=SMBIOS.SMBIOS_DMA.MemorySize;
  end else begin
    SMBIOS.LoadRawMemoryFromFile(AFilename);
    StartAddress:=SMBIOS.RAW_DMA.StartAddress;
    Size:=SMBIOS.RAW_DMA.MemorySize;
  end;
  ReadData;
end;

procedure TappRBE.OpenLocal;
begin
  try
    mmSave.Enabled:=True;
    mmSaveRawMemory.Enabled:=True;
    SMBIOS.ReadLocalMemory:=True;
    SMBIOS.RefreshData;
    StartAddress:=SMBIOS.SMBIOS_DMA.StartAddress;
    Size:=SMBIOS.SMBIOS_DMA.MemorySize;
  finally
    ReadData;
  end;
end;

procedure TappRBE.cmSaveDump(Sender: TObject);
var
  b: TBytes;
begin
  sd.Filename:=Format('%s %s %s %s - SMBIOS_%d_%d_%d_%d',[Trim(SMBIOS.SystemModel),Trim(SMBIOS.BIOSVendor),Trim(SMBIOS.BIOSVersion),Trim(SMBIOS.BIOSDate),SMBIOS.MajorVersion,SMBIOS.MinorVersion,SMBIOS.RevisionMajor,SMBIOS.RevisionMinor]);
  sd.Filename:=StringReplace(sd.Filename,',',' ',[rfReplaceAll]);
  sd.Filename:=StringReplace(sd.Filename,'/','-',[rfReplaceAll]);
  if Trim(sd.Filename)='' then
    sd.Filename:='unknown';
  sd.Filename:=sd.FileName+extSMBIOS;
  if not sd.Execute then
    Exit;
  b:=TBytes.Create($53,$4D,$42,$49,$4F,$53,0,SMBIOS.MajorVersion,SMBIOS.MinorVersion,StrToInt(Format('$%d%d',[SMBIOS.RevisionMajor,SMBIOS.RevisionMinor])),Lo(SMBIOS.StructCount),Hi(SMBIOS.StructCount),0,0,0,0);
  SMBIOS.SMBIOS_DMA.SaveToFile(sd.FileName,b);
end;

procedure TappRBE.mmOpenClick(Sender: TObject);
begin
  if od.Execute then
    OpenFile(od.FileName);
end;

procedure TappRBE.mmSaveRawMemoryClick(Sender: TObject);
begin
  rd.Filename:=Format('%s %s %s %s - SMBIOS_%d_%d_%d_%d',[Trim(SMBIOS.SystemModel),Trim(SMBIOS.BIOSVendor),Trim(SMBIOS.BIOSVersion),Trim(SMBIOS.BIOSDate),SMBIOS.MajorVersion,SMBIOS.MinorVersion,SMBIOS.RevisionMajor,SMBIOS.RevisionMinor]);
  rd.Filename:=StringReplace(rd.Filename,',',' ',[rfReplaceAll]);
  rd.Filename:=StringReplace(rd.Filename,'/','-',[rfReplaceAll]);
  if Trim(rd.Filename)='' then
    rd.Filename:='unknown';
  rd.Filename:=rd.FileName+extRaw;
  if not rd.Execute then
    Exit;
  SMBIOS.RAW_DMA.SaveToFile(rd.FileName,nil);
end;

end.
