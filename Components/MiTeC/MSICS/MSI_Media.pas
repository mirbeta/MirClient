{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Media Detection Part                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Media;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.MMSystem,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, MMSystem, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs;

const
  StorageFolderName = 'Media';

type
  TMiTeC_Media = class(TMiTeC_Component)
  private
    FDevice,
    FAUX,
    FMIDIIn,
    FMixer,
    FWAVEOut,
    FWAVEIn,
    FMIDIOut: TStrings;
    FSCL: string;
    FSCN: string;
    FSCM: string;
    FSCH: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    property SoundCardName: string read FSCN stored False;
    property SoundCardManufacturer: string read FSCM stored False;
    property SoundCardLocation: string read FSCL stored False;
    property SoundCardHardwareID: string read FSCH stored False;
    property Devices :TStrings read FDevice stored false;
    property WAVEIn :TStrings read FWAVEIn stored false;
    property WAVEOut :TStrings read FWAVEOut stored false;
    property MIDIIn :TStrings read FMIDIIn stored false;
    property MIDIOut :TStrings read FMIDIOut stored false;
    property AUX :TStrings read FAUX stored false;
    property Mixer :TStrings read FMixer stored false;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$ELSE}
     Registry,
     {$ENDIF}
     MiTeC_Routines, MiTeC_CfgMgrSetupAPI, MiTeC_RegUtils;

{ TMiTeC_Media }

procedure TMiTeC_Media.RefreshData;
var
  i,c,k,p,pci,dev,func: integer;
  s,l,h: string;
  dinfo: TSPDevInfoData;
  intf: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  n,pt: Cardinal;
  hdev: HDEVINFO;
  buf: array[0..{$IFDEF UNICODE}512{$ELSE}255{$ENDIF}] of byte;
  WOC :TWAVEOutCaps;
  WIC :TWAVEInCaps;
  MOC :TMIDIOutCaps;
  MIC :TMIDIInCaps;
  AXC :TAUXCaps;
  MXC :TMixerCaps;
const
  rv = 'DriverDesc';
  rvMediaClass = 'MEDIA';
begin
  inherited;
  Clear;
  s:='';
  k:=maxint;
  hdev:=SetupDiGetClassDevs(nil,nil,0,DIGCF_ALLCLASSES);
  if (INVALID_HANDLE_VALUE<>hdev) then
    try
      i:=0;
      pt:=0;
      dinfo.cbSize:=sizeof(TSPDevInfoData);
      while SetupDiEnumDeviceInfo(hDev,i,dinfo) do begin
        {$IFDEF RAD7PLUS}
        if Assigned(SetupDiGetDeviceProperty) then begin
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Class,pt,@buf,sizeof(buf),nil,0);
          s:=string(PChar(@buf));
          if SameText(s,'MEDIA') then begin
            ZeroMemory(@buf,sizeof(buf));
            SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DeviceDesc,pt,@buf,sizeof(buf),nil,0);
            s:=string(PChar(@buf));
            FDevice.Add(s);
            ZeroMemory(@buf,sizeof(buf));
            SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_HardwareIds,pt,@buf,sizeof(buf),nil,0);
            h:=string(PChar(@buf));
            ZeroMemory(@buf,sizeof(buf));
            SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_LocationInfo,pt,@buf,sizeof(buf),nil,0);
            l:=string(PChar(@buf));
            p:=Pos('AUDIO',h);
            GetLocation(l,pci,dev,func);
            if (pci>-1) or (p>0) then
              if (func<k) or (p>0) then begin
                k:=func;
                FSCN:=s;
                FSCH:=h;
                FSCL:=l;
                ZeroMemory(@buf,sizeof(buf));
                SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Manufacturer,pt,@buf,sizeof(buf),nil,0);
                FSCM:=string(PChar(@buf));
              end;
          end;
        end else
        {$ENDIF}
        begin
          intf.cbSize:=sizeof(TSPDeviceInterfaceData);
          if SetupDiCreateDeviceInterface(hDev,dinfo,dinfo.ClassGuid,nil,0,@intf) then begin
            n:=0;
            SetupDiGetDeviceInterfaceDetail(hdev,@intf,nil,0,n,nil);
            if (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
              pdidd:=AllocMem(n);
              try
                pdidd.cbSize:=SizeOf(TSPDeviceInterfaceDetailData);
                dinfo.cbSize:=sizeof(TSPDevInfoData);
                if (SetupDiGetDeviceInterfaceDetail(hdev,@intf,pdidd,n,n,@dinfo)) then begin
                  s:=GetString(hdev,dinfo,SPDRP_CLASS);
                  if SameText(s,'MEDIA') then begin
                    s:=GetString(hdev,dinfo,SPDRP_DEVICEDESC);
                    FDevice.Add(s);
                    h:=ExtractFilename(GetString(hdev,dinfo,SPDRP_HARDWAREID));
                    p:=Pos('AUDIO',h);
                    GetLocation(l,pci,dev,func);
                    if (pci>-1) or (p>0) then
                      if (func<k) or (p>0) then begin
                        k:=func;
                        FSCN:=s;
                        FSCH:=h;
                        FSCL:=l;
                        FSCM:=GetString(hdev,dinfo,SPDRP_MFG);
                      end;
                  end;
                end;
                finally
                  FreeMem(pdidd);
                end;
              end;
          end;
        end;
        inc(i);
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;

  //WOC:=AllocMem(SizeOf(TWAVEOutCaps));
  try
    c:=waveOutGetNumDevs;
    for i:=0 to c-1 do
      if WAVEOutGetDevCaps(i,@WOC,SizeOf(TWAVEOutCaps))=MMSYSERR_NOERROR then begin
        s:=WOC.szPname+' v'+inttostr(hi(WOC.vDriverVersion))+'.'+inttostr(hi(WOC.vDriverVersion));
        if FWaveOut.IndexOf(s)=-1 then
          FWAVEOut.Add(s);
      end;
  finally
    //FreeMem(WOC);
  end;

  //WIC:=Allocmem(SizeOf(TWAVEInCaps));
  try
    c:=waveInGetNumDevs;
    for i:=0 to c-1 do
      if WAVEinGetDevCaps(i,@WIC,SizeOf(TWAVEInCaps))=MMSYSERR_NOERROR then begin
        s:=WIC.szPname+' v'+inttostr(hi(WIC.vDriverVersion))+'.'+inttostr(hi(WIC.vDriverVersion));
        if FWaveIn.IndexOf(s)=-1 then
          FWAVEIn.Add(s);
      end;
  finally
    //Freemem(WIC);
  end;

  //MOC:=Allocmem(SizeOf(TMIDIOutCaps));
  try
    c:=midiOutGetNumDevs;
    for i:=0 to c-1 do
      if MIDIoutGetDevCaps(i,@MOC,SizeOf(TMIDIOutCaps))=MMSYSERR_NOERROR then begin
        s:=MOC.szPname+' v'+inttostr(hi(MOC.vDriverVersion))+'.'+inttostr(hi(MOC.vDriverVersion));
        if FMIDIOut.IndexOf(s)=-1 then
          FMIDIout.Add(s);
      end;
  finally
    //Freemem(MOC);
  end;

  //MIC:=Allocmem(SizeOf(TMIDIInCaps));
  try
    c:=midiInGetNumDevs;
    for i:=0 to c-1 do
      if MIDIinGetDevCaps(i,@MIC,SizeOf(TMIDIInCaps))=MMSYSERR_NOERROR then begin
        s:=MIC.szPname+' v'+inttostr(hi(MIC.vDriverVersion))+'.'+inttostr(hi(MIC.vDriverVersion));
        if FMIDIIn.IndexOf(s)=-1 then
          FMIDIin.Add(s);
      end;
  finally
    //Freemem(MIC);
  end;

  //AXC:=Allocmem(SizeOf(TAUXCaps));
  try
    c:=auxGetNumDevs;
    for i:=0 to c-1 do
      if AUXGetDevCaps(i,@AXC,SizeOf(TAUXCaps))=MMSYSERR_NOERROR then begin
        s:=AXC.szPname+' v'+inttostr(hi(AXC.vDriverVersion))+'.'+inttostr(hi(AXC.vDriverVersion));
        if FAUX.IndexOf(s)=-1 then
          FAUX.Add(s);
      end;
  finally
    //Freemem(AXC);
  end;

  //MXC:=Allocmem(SizeOf(TMixerCaps));
  try
    c:=mixerGetNumDevs;
    for i:=0 to c-1 do
      if MixerGetDevCaps(i,@MXC,SizeOf(TMixerCaps))=MMSYSERR_NOERROR then begin
        s:=MXC.szPname+' v'+inttostr(hi(MXC.vDriverVersion))+'.'+inttostr(hi(MXC.vDriverVersion));
        if FMixer.IndexOf(s)=-1 then
          FMixer.Add(s);
      end;
  finally
    //Freemem(MXC);
  end;

  SetDataAvail(True);
end;

procedure TMiTeC_Media.Clear;
begin
  FDevice.Clear;
  FWaveIn.Clear;
  FWaveOut.Clear;
  FMIDIIn.Clear;
  FMIDIOut.Clear;
  FMixer.Clear;
  FAUX.Clear;
  FSCN:='';
  FSCM:='';
  FSCL:='';
  FSCH:='';
end;

constructor TMiTeC_Media.Create;
begin
  inherited Create(AOwner);
  FDevice:=TStringList.Create;
  FWaveIn:=TStringList.Create;
  FWaveOut:=TStringList.Create;
  FMIDIIn:=TStringList.Create;
  FMIDIOut:=TStringList.Create;
  FMixer:=TStringList.Create;
  FAUX:=TStringList.Create;
end;

destructor TMiTeC_Media.Destroy;
begin
  FDevice.Free;
  FWaveIn.Free;
  FWaveOut.Free;
  FMIDIIn.Free;
  FMIDIOut.Free;
  FMixer.Free;
  FAUX.Free;
  inherited;
end;

function TMiTeC_Media.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
    try
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FDevice.CommaText:=ReadStrProperty(sl,'Devices');
            Self.FAUX.CommaText:=ReadStrProperty(sl,'AUX');
            Self.FMIDIIn.CommaText:=ReadStrProperty(sl,'MIDIIn');
            Self.FMixer.CommaText:=ReadStrProperty(sl,'Mixer');
            Self.FWAVEOut.CommaText:=ReadStrProperty(sl,'WaveOut');
            Self.FWAVEIn.CommaText:=ReadStrProperty(sl,'WaveIn');
            Self.FMIDIOut.CommaText:=ReadStrProperty(sl,'MIDIOut');
            Self.FSCN:=ReadStrProperty(sl,'SoundCardName');
            Self.FSCM:=ReadStrProperty(sl,'SoundCardManufacturer');
            Self.FSCH:=ReadStrProperty(sl,'SoundCardHardwareID');
            Self.FSCL:=ReadStrProperty(sl,'SoundCardLocation');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Media.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'Devices',Self.Devices.CommaText);
        WriteStrProperty(sl,'MIDIIn',Self.MIDIIn.CommaText);
        WriteStrProperty(sl,'MIDIOut',Self.MIDIOut.CommaText);
        WriteStrProperty(sl,'Mixer',Self.Mixer.CommaText);
        WriteStrProperty(sl,'WaveIn',Self.WAVEIn.CommaText);
        WriteStrProperty(sl,'WaveOut',Self.WAVEOut.CommaText);
        WriteStrProperty(sl,'AUX',Self.AUX.CommaText);
        WriteStrProperty(sl,'SoundCardName',Self.SoundCardName);
        WriteStrProperty(sl,'SoundCardManufacturer',Self.SoundCardManufacturer);
        WriteStrProperty(sl,'SoundCardHardwareID',Self.SoundCardHardwareID);
        WriteStrProperty(sl,'SoundCardLocation',Self.SoundCardLocation);
        //WriteIntProperty(sl,'GamePortIndex',Self.GamePortIndex);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.

