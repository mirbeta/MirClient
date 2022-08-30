{$INCLUDE ..\..\..\Compilers.Inc}



unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_Common, MSI_SystemInfo, ComCtrls, StdCtrls;

type
  TwndMain = class(TForm)
    SI: TMiTeC_SystemInfo;
    bSave: TButton;
    cbxZLIB: TCheckBox;
    bLoad: TButton;
    List: TListView;
    cbxCrypt: TCheckBox;
    lRes: TLabel;
    lTS: TLabel;
    Label1: TLabel;
    procedure bSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure SIReadHeader(const AFilename: string; AHeader: Pointer);
    procedure SIWriteHeader(const AFilename: string; AFormat: Integer;
      const AComment: string);
    procedure Label1Click(Sender: TObject);
  private
    procedure FillList;
  public
  end;

var
  wndMain: TwndMain;

implementation

uses ShellAPI, MSI_Defs, MiTeC_Routines, MSI_SMBIOS, MiTeC_ZLIB_FPC, MiTeC_WinCrypt, ComObj, ActiveX, MiTeC_SS;

{$R *.lfm}

type
  PCustomHeader = ^TCustomHeader;
  TCustomHeader = record
    Format: Integer;
    Timestamp: TDateTime;
    Author: string;
  end;

procedure EncryptStream(InStream, OutStream: TStream);
begin
  CryptStream(InStream,OutStream,'password',True);
end;

procedure DecryptStream(InStream, OutStream: TStream);
begin
  CryptStream(InStream,OutStream,'password',False);
end;

procedure CompressStream(InStream, OutStream: TStream);
{$if not defined(RAD7PLUS) and not defined(FPC)}
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
begin
  InpBuf:=nil;
  OutBuf:=nil;
  try
    GetMem(InpBuf,InStream.Size);
    InStream.Position:=0;
    InpBytes:=InStream.Read(InpBuf^,InStream.Size);
    CompressBuf(InpBuf, InpBytes,OutBuf,OutBytes);
    OutStream.Write(OutBuf^,OutBytes);
  finally
    if InpBuf<>nil then
      FreeMem(InpBuf);
    if OutBuf<>nil then
      FreeMem(OutBuf);
  end;
{$ELSE}
begin
  InStream.Position:=0;
  ZCompressStream(InStream, OutStream, zcMax);
{$ifend}
 OutStream.Position:=0;
end;

procedure DecompressStream(InStream, OutStream: TStream);
{$if not defined(RAD7PLUS) and not defined(FPC)}
var
  InpBuf, OutBuf: Pointer;
  OutBytes, Size: Integer;
begin
  InStream.Position:=0;
  InpBuf:=nil;
  OutBuf:=nil;
  Size:=InStream.Size-InStream.Position;
  if Size>0 then
    try
      GetMem(InpBuf,Size);
      InStream.Read(InpBuf^,Size);
      DecompressBuf(InpBuf,Size,0,OutBuf, OutBytes);
      OutStream.Write(OutBuf^,OutBytes);
    finally
      if InpBuf<>nil then
        FreeMem(InpBuf);
      if OutBuf<>nil then
        FreeMem(OutBuf);
    end;
  OutStream.Position:=0;
{$ELSE}
begin
  InStream.Position:=0;
  ZDecompressStream(InStream, OutStream);
  OutStream.Position:=0;
{$ifend}
end;

procedure CompressEncryptStream(InStream, OutStream: TStream);
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    CompressStream(InStream,ms);
    ms.Position:=0;
    EncryptStream(ms,OutStream);
  finally
    ms.Free;
  end;
end;

procedure DecryptDecompressStream(InStream, OutStream: TStream);
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    DecryptStream(InStream,ms);
    ms.Position:=0;
    DecompressStream(ms,OutStream);
  finally
    ms.Free;
  end;
end;

procedure TwndMain.bSaveClick(Sender: TObject);
var
  fn: string;
  t: Int64;
  wh: Boolean;
begin
  Screen.Cursor:=crHourglass;
  try
    wh:=True;
    fn:=ChangeFileExt(Application.ExeName,'.cff'); //custom format file :)
    if FileExists(fn) then
      DeleteFile(fn);
    t:=GetTickCount64;
    if cbxCrypt.Checked and not cbxZLIB.Checked then
      SI.SaveToStorage(fn,wh,1,'',EncryptStream)
    else if not cbxCrypt.Checked and cbxZLIB.Checked then
      SI.SaveToStorage(fn,wh,2,'',CompressStream)
    else if cbxCrypt.Checked and cbxZLIB.Checked then
      SI.SaveToStorage(fn,wh,3,'',CompressEncryptStream)
    else
      SI.SaveToStorage(fn,wh);
    lRes.Caption:=Format('Elapsed time: %1.2f s (%1.0n B)',[(GetTickCount64-t)/1000,GetFileSize(fn)/1]);
    bLoad.Enabled:=True;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TwndMain.bLoadClick(Sender: TObject);
var
  fn: string;
  t: Int64;
  ch: TCustomHeader;
  rh: Boolean;
begin
  Screen.Cursor:=crHourglass;
  try
    fn:=ChangeFileExt(Application.ExeName,'.cff'); //custom format file :)
    SI.LoadStorageInfo(fn,@ch);
    t:=GetTickCount64;
    rh:=False;
    case ch.Format of
      0: SI.LoadFromStorage(fn,rh);
      1: SI.LoadFromStorage(fn,rh,DecryptStream);
      2: SI.LoadFromStorage(fn,rh,DecompressStream);
      3: SI.LoadFromStorage(fn,rh,DecryptDecompressStream);
    end;
    lRes.Caption:=Format('Elapsed time: %1.2f s',[(GetTickCount64-t)/1000]);
    FillList;
    bSave.Enabled:=False;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TwndMain.FillList;
var
  c,i,idx: Integer;
begin
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    if SI.OS.DataAvailable and (SI.OS.OSName<>'') then begin
      with List.Items.Add do begin
        Caption:='Machine';
        if SI.Machine.BIOS.BIOSDataCount>0 then
          SubItems.Add(SI.Machine.BIOS.BIOSValue['SystemProductName'].Value)
        else
          SubItems.Add(SI.Machine.SMBIOS.SystemModel);
      end;
      with List.Items.Add do begin
        Caption:='CPU';
        SubItems.Add(Format('%d x %s - %d MHz',[SI.CPU.CPUPhysicalCount,SI.CPU.CPUName,SI.CPU.Frequency]));
      end;
      with List.Items.Add do begin
        Caption:='Memory';
        if SI.Machine.SMBIOS.MemoryDeviceCount>0 then begin
          c:=0;
          idx:=-1;
          for i:=0 to SI.Machine.SMBIOS.MemoryDeviceCount-1 do
            if SI.Machine.SMBIOS.MemoryDevice[i].Size>0 then begin
              Inc(c);
              if idx=-1 then
                idx:=i;
            end;
          SubItems.Add(Format('%d x %d MB %s',[c,
                                               SI.Machine.SMBIOS.MemoryDevice[idx].Size,
                                               MemoryDeviceTypes[SI.Machine.SMBIOS.MemoryDevice[idx].Device]]))
        end else
          SubItems.Add(Format('%d MB',[SI.Memory.PhysicalTotal shr 20]));
      end;
      for i:=0 to SI.Display.AdapterCount-1 do
        with List.Items.Add do begin
          Caption:='Graphics';
          if SI.Display.Adapter[i].Memory>0 then
            SubItems.Add(Format('%s - %d MB',[SI.Display.Adapter[i].Name,SI.Display.Adapter[i].Memory shr 20]))
          else
            SubItems.Add(SI.Display.Adapter[i].Name);
        end;
      with List.Items.Add do begin
        Caption:='OS';
        SubItems.Add(Format('%s %s',[SI.OS.OSName,SI.OS.OSEdition]));
      end;
    end else
      List.Items.Add.Caption:='No data available';
  finally
    List.Items.EndUpdate;
  end;
end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  SI.RefreshData([soMachine,soCPU,soOS,soDisplay,soMemory]);
  FillList;
end;

procedure TwndMain.Label1Click(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.mitec.cz/ssv.html',nil,nil,SW_SHOWDEFAULT);
end;

procedure TwndMain.SIReadHeader(const AFilename: string; AHeader: Pointer);
var
  stg: IStorage;
  SS: TStructuredStorage;
  SPS: TStoragePropertySet;
begin
  if not Assigned(AHeader) then
    Exit;

  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SPS:=nil;
    if SS<>nil then
      SPS:=SS.OpenPropertySet(StringToGUID(FMTID_SummaryInformation),STG_READ_INSTORAGE,False);
    if SPS<>nil then
      try
        PCustomHeader(AHeader)^.Timestamp:=ReadDatetimeProperty(SPS._IPropertyStorage,PIDSI_CREATE_DTM);
        PCustomHeader(AHeader)^.Author:=ReadStringProperty(SPS._IPropertyStorage,PIDSI_AUTHOR);
        PCustomHeader(AHeader)^.Format:=ReadIntegerProperty(SPS._IPropertyStorage,'Format');

        lTS.Caption:=DateTimeToStr(PCustomHeader(AHeader)^.Timestamp);
      finally
        SPS.Free;
      end;
  finally
    SS.Free;
  end;
end;

procedure TwndMain.SIWriteHeader(const AFilename: string; AFormat: Integer;
  const AComment: string);
var
  stg: IStorage;
  SS: TStructuredStorage;
  SPS: TStoragePropertySet;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try (SS._IStorage as IPropertySetStorage).Delete(StringToGUID(FMTID_SummaryInformation)) except end;
    SPS:=SS.OpenPropertySet(StringToGUID(FMTID_SummaryInformation),STG_OPEN,True);
    try
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_TITLE,'Customized header');
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_AUTHOR,'Michal Mutl');
      WriteDateTimeProperty(SPS._IPropertyStorage,PIDSI_CREATE_DTM,now);
      WriteIntegerProperty(SPS._IPropertyStorage,'Format',AFormat);
    finally
      SPS.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.
