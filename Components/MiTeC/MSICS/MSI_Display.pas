{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Display Detection Part                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Display;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs{, MSI_Devices};

const
  StorageFolderName = 'Display';

type
  TDisplayAdapter = record
    Name,
    DAC,
    Chipset,
    BIOS: string;
    Memory: int64;
    Service,
    Driver,
    DevPar: string;
  end;
  TDisplayAdapters = array of TDisplayAdapter;

const
  _SHADEBLENDCAPS = 120; // Shading and blending caps
  {$EXTERNALSYM _SHADEBLENDCAPS}
  _COLORMGMTCAPS  = 121; // Color Management caps
  {$EXTERNALSYM _COLORMGMTCAPS}

  // Shading and blending caps

  SB_NONE          = $00000000;
  {$EXTERNALSYM SB_NONE}
  SB_CONST_ALPHA   = $00000001;
  {$EXTERNALSYM SB_CONST_ALPHA}
  SB_PIXEL_ALPHA   = $00000002;
  {$EXTERNALSYM SB_PIXEL_ALPHA}
  SB_PREMULT_ALPHA = $00000004;
  {$EXTERNALSYM SB_PREMULT_ALPHA}

  SB_GRAD_RECT = $00000010;
  {$EXTERNALSYM SB_GRAD_RECT}
  SB_GRAD_TRI  = $00000020;
  {$EXTERNALSYM SB_GRAD_TRI}

// Color Management caps

  CM_NONE       = $00000000;
  {$EXTERNALSYM CM_NONE}
  CM_DEVICE_ICM = $00000001;
  {$EXTERNALSYM CM_DEVICE_ICM}
  CM_GAMMA_RAMP = $00000002;
  {$EXTERNALSYM CM_GAMMA_RAMP}
  CM_CMYK_COLOR = $00000004;
  {$EXTERNALSYM CM_CMYK_COLOR}

type
  TCurveCap = (ccCircles,ccPieWedges,ccChords,ccEllipses,ccWideBorders,ccStyledBorders,
               ccWideStyledBorders,ccInteriors,ccRoundedRects);
  TLineCap = (lcPolylines,lcMarkers,lcMultipleMarkers,lcWideLines,lcStyledLines,
               lcWideStyledLines,lcInteriors);
  TPolygonCap = (pcAltFillPolygons,pcRectangles,pcWindingFillPolygons,pcSingleScanlines,
                 pcWideBorders,pcStyledBorders,pcWideStyledBorders,pcInteriors);
  TRasterCap = (rcRequiresBanding,rcTransferBitmaps,rcBitmaps64K,rcSetGetDIBits,
                rcSetDIBitsToDevice,rcFloodfills,rcWindows2xFeatures,rcPaletteBased,
                rcScaling,rcStretchBlt,rcStretchDIBits);
  TTextCap = (tcCharOutPrec,tcStrokeOutPrec,tcStrokeClipPrec,tcCharRotation90,
              tcCharRotationAny,tcScaleIndependent,tcDoubledCharScaling,tcIntMultiScaling,
              tcAnyMultiExactScaling,tcDoubleWeightChars,tcItalics,tcUnderlines,
              tcStrikeouts,tcRasterFonts,tcVectorFonts,tcNoScrollUsingBlts);

  TShadeBlendCap = (sbcConstAlpha,sbcGradRect,sbcGradTri, sbcPixelAlpha,sbcPremultAlpha);

  TColorMgmtCap = (cmcCMYKColor, cmcDeviceICM, cmcGammaRamp);

  TCurveCaps = set of TCurveCap;
  TLineCaps = set of TLineCap;
  TPolygonCaps = set of TPolygonCap;
  TRasterCaps = set of TRasterCap;
  TTextCaps = set of TTextCap;
  TShadeBlendCaps = set of TShadeBlendCap;
  TColorMgmtCaps = set of TColorMgmtCap;

  TMiTeC_Display = class(TMiTeC_Component)
  private
    FVertRes: integer;
    FColorDepth: integer;
    FHorzRes: integer;
    FBIOSDate: string;
    FBIOSVersion: string;
    FPixelDiagonal: integer;
    FPixelHeight: integer;
    FVertSize: integer;
    FPixelWidth: integer;
    FHorzSize: integer;
    FTechnology: string;
    FCurveCaps: TCurveCaps;
    FLineCaps: TLineCaps;
    FPolygonCaps: TPolygonCaps;
    FRasterCaps: TRasterCaps;
    FTextCaps: TTextCaps;
    FVidModes: TStrings;
    FFontSize: Cardinal;
    FVRR: Cardinal;
    FBIOSString: string;
    FDDV: integer;
    FShadeBlendCaps: TShadeBlendCaps;
    FColorMgmtCaps: TColorMgmtCaps;
    FAdapters: TDisplayAdapters;
    //FDevices: TMiTeC_Devices;
    function GetAdapter(AIndex: Integer): TDisplayAdapter;
    function GetAdapterCount: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property AdapterCount: integer read GetAdapterCount;
    property Adapter[AIndex: Integer]: TDisplayAdapter read GetAdapter;
  published
    //property MiTeC_Devices: TMiTeC_Devices read FDevices write FDevices;
    property HorzRes: integer read FHorzRes stored false;
    property VertRes: integer read FVertRes stored false;
    property HorzSize: integer read FHorzSize stored false;
    property VertSize: integer read FVertSize stored false;
    property ColorDepth: integer read FColorDepth stored false;
    property DeviceDriverVersion: integer read FDDV stored false;

    property BIOSVersion: string read FBIOSVersion stored false;
    property BIOSDate: string read FBIOSDate stored false;
    property BIOSString: string read FBIOSString stored false;

    property Technology: string read FTechnology stored false;
    property PixelWidth: integer read FPixelWidth stored false;
    property PixelHeight: integer read FPixelHeight stored false;
    property PixelDiagonal: integer read FPixelDiagonal stored false;
    property RasterCaps: TRasterCaps read FRasterCaps stored false;
    property CurveCaps: TCurveCaps read FCurveCaps stored false;
    property LineCaps: TLineCaps read FLineCaps stored false;
    property PolygonCaps: TPolygonCaps read FPolygonCaps stored false;
    property TextCaps: TTextCaps read FTextCaps stored false;
    property ShadeBlendCaps: TShadeBlendCaps read FShadeBlendCaps stored false;
    property ColorMgmtCaps: TColorMgmtCaps read FColorMgmtCaps stored false;
    property Modes: TStrings read FVidModes stored False;
    property FontResolution: Cardinal read FFontSize stored False;
    property VerticalRefreshRate: Cardinal read FVRR stored False;
  end;

procedure GetCurveCapsStr(CurveCaps :TCurveCaps; ACaps :TStringList);
procedure GetLineCapsStr(LineCaps :TLineCaps; ACaps :TStringList);
procedure GetPolygonCapsStr(PolygonCaps :TPolygonCaps; ACaps :TStringList);
procedure GetRasterCapsStr(RasterCaps :TRasterCaps; ACaps :TStringList);
procedure GetTextCapsStr(TextCaps :TTextCaps; ACaps :TStringList);
procedure GetShadeBlendCapsStr(ShadeBlendCaps :TShadeBlendCaps; ACaps :TStringList);
procedure GetColorMgmtCapsStr(ColorMgmtCaps :TColorMgmtCaps; ACaps :TStringList);

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.Math,
     {$ELSE}
     Registry, Math,
     {$ENDIF}
     MiTeC_Routines, MiTeC_StrUtils, MiTeC_CfgMgrSetupAPI, MiTeC_RegUtils;

{ TMiTeC_Display }

function GetDeviceParams(AKey: string): string;
var
  sl: TStringList;
  i: integer;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(AKey+'\Device Parameters',False) then begin
        sl:=TStringList.Create;
        try
          GetValueNames(sl);
          for i:=0 to sl.Count-1 do
            if (sl[i]<>'') and (GetDataType(sl[i])=rdString) then begin
              Result:=ReadString(sl[i]);
              Break;
            end;
        finally
          sl.Free;
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure GetDisplayInfo(ADeviceParam,AServiceName: string; var InfoRecord: TDisplayAdapter);
var
  StrData :PChar;
  IntData: array[0..254] of Byte;
  rk: string;
  ds: Integer;
  r: TRegistry;
const
  rk1 = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Services\%s\Device0';
  rk2 = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\Video\%s\0000';
  rvDAC = 'HardwareInformation.DacType';
  rvChip = 'HardwareInformation.ChipType';
  rvqwMem = 'HardwareInformation.qwMemorySize';
  rvMem = 'HardwareInformation.MemorySize';
  rvBIOS = 'HardwareInformation.BiosString';
begin
  //ResetMemory(InfoRecord,SizeOf(InfoRecord));
  r:=OpenRegistryReadOnly;
  with r do begin
    RootKey:=HKEY_LOCAL_MACHINE;
    rk:=Format(rk1,[AServiceName]);
    if not(OpenKey(rk,False) and ValueExists(rvDAC)) then
      rk:=Format(rk2,[ADeviceparam]);
    CloseKey;
    if OpenKey(rk,False) then begin
      StrData:=StrAlloc(255);
      if ValueExists(rvDAC) then
        try
          if GetDataType(rvDAC) in [rdString, rdExpandString] then
            InfoRecord.DAC:=ReadString(rvDAC)
          else begin
            ReadBinaryData(rvDAC,StrData^,255);
            InfoRecord.DAC:=WideCharToString(PWideChar(StrData));
          end;
        except
        end;
      if ValueExists(rvChip) then
        try
          if GetDataType(rvChip) in [rdString, rdExpandString] then
            InfoRecord.Chipset:=ReadString(rvChip)
          else begin
            ReadBinaryData(rvChip,StrData^,255);
            InfoRecord.Chipset:=WideCharToString(PWideChar(StrData));
          end;
        except
        end;
      if ValueExists(rvBIOS) then
        try
          if GetDataType(rvBIOS) in [rdString, rdExpandString] then
            InfoRecord.BIOS:=ReadString(rvBIOS)
          else begin
            ReadBinaryData(rvBIOS,StrData^,255);
            InfoRecord.BIOS:=WideCharToString(PWideChar(StrData));
          end;
        except
        end;
      if ValueExists(rvMem) then
        try
          case GetDataType(rvMem) of
            rdInteger: InfoRecord.Memory:=ReadCardinal(r,rvMem);
            rdString, rdExpandString: InfoRecord.Memory:=StrToInt64Def(ReadString(rvMem),0);
            else begin
              ds:=GetDataSize(rvMem);
              ReadBinaryData(rvMem,IntData,ds);
              Move(IntData,InfoRecord.Memory,ds);
            end;
          end;
        except
        end;
      if ValueExists(rvqwMem) then
        try
          case GetDataType(rvqwMem) of
            rdInteger: InfoRecord.Memory:=ReadCardinal(r,rvqwMem);
            rdString, rdExpandString: InfoRecord.Memory:=StrToInt64Def(ReadString(rvqwMem),0);
            else begin
              ds:=GetDataSize(rvqwMem);
              ReadBinaryData(rvqwMem,IntData,ds);
              Move(IntData,InfoRecord.Memory,ds);
            end;
          end;
        except
        end;
      StrDispose(StrData);
      CloseKey;
    end;
    Free;
  end;
end;

procedure GetVideoBIOSInfo(var Version, Date: string);
var
  sl: TStringList;
const
  rk = {HKEY_LOCAL_MACHINE}'\HARDWARE\DESCRIPTION\System';
  rvVideoBiosDate = 'VideoBiosDate';
  rvVideoBiosVersion = 'VideoBiosVersion';
begin
  Version:='';
  sl:=TStringList.Create;
  try
    sl.Text:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rk,rvVideoBiosVersion,False);
    if sl.Count>0 then
      Version:=sl[0];
  finally
    sl.Free;
  end;
  Date:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rk,rvVideoBiosDate,False);
end;

procedure TMiTeC_Display.RefreshData;
var
  i :integer;
  DevMode : TDevMode;
  InfoRec: TDisplayAdapter;
  s,rk,rlv,rlk,li: string;
  lDC: hDC;
  guid: TGUID;
  did: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  dinfo: TSPDevInfoData;
  hdev: HDEVINFO;
  le,n: Cardinal;
begin
  inherited;
  Clear;
  {
  if Assigned(FDevices) then
    with FDevices do begin
      if not (Owner is TMiTeC_Component) or not FDevices.DataAvailable then
        RefreshData;
      for i:=0 to DeviceCount-1 do
        if Devices[i].DeviceClass=dcDisplay then
          if ((Devices[i].ResourceListKey<>'') and (Devices[i].Location<>'')) then begin
            ResetMemory(InfoRec,SizeOf(InfoRec));
            with InfoRec do begin
              Name:=Devices[i].Name;
              Service:=Devices[i].Service;
              Driver:=Devices[i].Driver;
              DevPar:=Devices[i].DeviceParam;
              GetDisplayInfo(DevPar,Service,InfoRec)
            end;
            SetLength(FAdapters,Length(FAdapters)+1);
            FAdapters[High(FAdapters)]:=InfoRec;
          end;
    end;
  }

  guid:=GUID_DEVINTERFACE_DISPLAY_ADAPTER;
  hdev:=SetupDiGetClassDevs(@guid,nil,0,DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then begin
    try
      for i:=0 to 99 do begin
        FillChar(did,SizeOf(did),0);
        did.cbSize:=SizeOf(did);
        if (SetupDiEnumDeviceInterfaces(hdev,nil,guid,i,did)) then begin
          n:=0;
          SetupDiGetDeviceInterfaceDetail(hdev,@did,nil,0,n,nil);
          le:=GetLastError;
          if (le=ERROR_INSUFFICIENT_BUFFER) then begin
            n:=n;
            pdidd:=AllocMem(n);
            try
              pdidd.cbSize:=SizeOf(TSPDeviceInterfaceDetailData);
              dinfo.cbSize:=sizeof(TSPDevInfoData);
              if (SetupDiGetDeviceInterfaceDetail(hdev,@did,pdidd,n,n,@dinfo)) then begin
                s:=PChar(@(pdidd.DevicePath));
                if (Trim(s)<>'') and (Trim(s)<>'\') then begin
                  rk:='\SYSTEM\CurrentControlSet\Enum\'+FastStringReplace(Copy(s,5,Pos('{',s)-5),'#','\');
                  li:=GetString(hdev,dinfo,SPDRP_LOCATION_INFORMATION);
                  GetResourceListLocation(rk,rlk,rlv);
                  if (rlk<>'') and (li<>'') then begin
                    ResetMemory(InfoRec,SizeOf(InfoRec));
                    with InfoRec do begin
                      Name:=GetString(hdev,dinfo,SPDRP_DEVICEDESC);
                      Service:=GetString(hdev,dinfo,SPDRP_SERVICE);
                      Driver:=GetString(hdev,dinfo,SPDRP_DRIVER);
                      DevPar:=GetDeviceParams(rk);
                      GetDisplayInfo(DevPar,Service,InfoRec);
                    end;
                    SetLength(FAdapters,Length(FAdapters)+1);
                    FAdapters[High(FAdapters)]:=InfoRec;
                  end;
                end;
              end;
            finally
              FreeMem(pdidd);
            end;
          end;
        end else begin
          le:=GetLastError;
          if le=ERROR_NO_MORE_ITEMS then
            Break;
        end;
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;
  end;

  GetVideoBIOSInfo(FBIOSVersion,FBIOSDate);

  lDC:=GetDC(0);
  try
  FDDV:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}DRIVERVERSION);
  FFontSize:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}LOGPIXELSY);
  FHorzRes:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}HORZRES);
  FVertRes:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}VERTRES);
  FHorzSize:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}HORZSIZE);
  FVertSize:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}VERTSIZE);
  FColorDepth:=GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows.{$ELSE}Windows.{$ENDIF}BITSPIXEL);

  if Win32Platform=VER_PLATFORM_WIN32_NT then
    FVRR:=GetDeviceCaps(lDC,VREFRESH)
  else
    FVRR:=0;
  case GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TECHNOLOGY) of
    DT_PLOTTER:    FTechnology:='Vector Plotter';
    DT_RASDISPLAY: FTechnology:='Raster Display';
    DT_RASPRINTER: FTechnology:='Raster Printer';
    DT_RASCAMERA:  FTechnology:='Raster Camera';
    DT_CHARSTREAM: FTechnology:='Character Stream';
    DT_METAFILE:   FTechnology:='Metafile';
    DT_DISPFILE:   FTechnology:='Display File';
  end;
  FPixelWidth:=GetDeviceCaps(lDC,ASPECTX);
  FPixelHeight:=GetDeviceCaps(lDC,ASPECTY);
  FPixelDiagonal:=GetDeviceCaps(lDC,ASPECTXY);
  if GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS)<>CC_NONE then begin
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_CIRCLES)=CC_CIRCLES then
      FCurveCaps:=FCurveCaps+[ccCircles];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_PIE)=CC_PIE then
      FCurveCaps:=FCurveCaps+[ccPieWedges];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_CHORD)=CC_CHORD then
      FCurveCaps:=FCurveCaps+[ccChords];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_ELLIPSES)=CC_ELLIPSES then
      FCurveCaps:=FCurveCaps+[ccEllipses];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_WIDE)=CC_WIDE then
      FCurveCaps:=FCurveCaps+[ccWideBorders];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_STYLED)=CC_STYLED then
      FCurveCaps:=FCurveCaps+[ccStyledBorders];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_WIDESTYLED)=CC_WIDESTYLED then
      FCurveCaps:=FCurveCaps+[ccWideStyledBorders];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_INTERIORS)=CC_INTERIORS then
      FCurveCaps:=FCurveCaps+[ccInteriors];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.CURVECAPS) and CC_ROUNDRECT)=CC_ROUNDRECT then
      FCurveCaps:=FCurveCaps+[ccRoundedRects];
  end;

  if GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS)<>LC_NONE then begin
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_POLYLINE)=LC_POLYLINE then
      FLineCaps:=FLineCaps+[lcPolylines];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_MARKER)=LC_MARKER then
      FLineCaps:=FLineCaps+[lcMarkers];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_POLYMARKER)=LC_POLYMARKER then
      FLineCaps:=FLineCaps+[lcMultipleMarkers];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_WIDE)=LC_WIDE then
      FLineCaps:=FLineCaps+[lcWideLines];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_STYLED)=LC_STYLED then
      FLineCaps:=FLineCaps+[lcStyledLines];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_WIDESTYLED)=LC_WIDESTYLED then
      FLineCaps:=FLineCaps+[lcWideStyledLines];
    if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.LINECAPS) and LC_INTERIORS)=LC_INTERIORS then
      FLineCaps:=FLineCaps+[lcInteriors];
  end;

  if GetDeviceCaps(lDC,POLYGONALCAPS)<>PC_NONE then begin
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_POLYGON)=PC_POLYGON then
      FPolygonCaps:=FPolygonCaps+[pcAltFillPolygons];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_RECTANGLE)=PC_RECTANGLE then
      FPolygonCaps:=FPolygonCaps+[pcRectangles];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_WINDPOLYGON)=PC_WINDPOLYGON then
      FPolygonCaps:=FPolygonCaps+[pcWindingFillPolygons];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_SCANLINE)=PC_SCANLINE then
      FPolygonCaps:=FPolygonCaps+[pcSingleScanlines];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_WIDE)=PC_WIDE then
      FPolygonCaps:=FPolygonCaps+[pcWideBorders];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_STYLED)=PC_STYLED then
      FPolygonCaps:=FPolygonCaps+[pcStyledBorders];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_WIDESTYLED)=PC_WIDESTYLED then
      FPolygonCaps:=FPolygonCaps+[pcWideStyledBorders];
    if (GetDeviceCaps(lDC,POLYGONALCAPS) and PC_INTERIORS)=PC_INTERIORS then
      FPolygonCaps:=FPolygonCaps+[pcInteriors];
  end;

  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_BANDING)=RC_BANDING then
    FRasterCaps:=FRasterCaps+[rcRequiresBanding];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_BITBLT)=RC_BITBLT then
    FRasterCaps:=FRasterCaps+[rcTransferBitmaps];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_BITMAP64)=RC_BITMAP64 then
    FRasterCaps:=FRasterCaps+[rcBitmaps64K];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_DI_BITMAP)=RC_DI_BITMAP then
    FRasterCaps:=FRasterCaps+[rcSetGetDIBits];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_DIBTODEV)=RC_DIBTODEV then
    FRasterCaps:=FRasterCaps+[rcSetDIBitsToDevice];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_FLOODFILL)=RC_FLOODFILL then
    FRasterCaps:=FRasterCaps+[rcFloodfills];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_GDI20_OUTPUT)=RC_GDI20_OUTPUT then
    FRasterCaps:=FRasterCaps+[rcWindows2xFeatures];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_PALETTE)=RC_PALETTE then
    FRasterCaps:=FRasterCaps+[rcPaletteBased];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_SCALING)=RC_SCALING then
    FRasterCaps:=FRasterCaps+[rcScaling];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_STRETCHBLT)=RC_STRETCHBLT then
    FRasterCaps:=FRasterCaps+[rcStretchBlt];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.RASTERCAPS) and RC_STRETCHDIB)=RC_STRETCHDIB then
    FRasterCaps:=FRasterCaps+[rcStretchDIBits];

  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_OP_CHARACTER)=TC_OP_CHARACTER then
    FTextCaps:=FTextCaps+[tcCharOutPrec];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_OP_STROKE)=TC_OP_STROKE then
    FTextCaps:=FTextCaps+[tcStrokeOutPrec];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_CP_STROKE)=TC_CP_STROKE then
    FTextCaps:=FTextCaps+[tcStrokeClipPrec];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_CR_90)=TC_CR_90 then
    FTextCaps:=FTextCaps+[tcCharRotation90];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_CR_ANY)=TC_CR_ANY then
    FTextCaps:=FTextCaps+[tcCharRotationAny];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_SF_X_YINDEP)=TC_SF_X_YINDEP then
    FTextCaps:=FTextCaps+[tcScaleIndependent];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_SA_DOUBLE)=TC_SA_DOUBLE then
    FTextCaps:=FTextCaps+[tcDoubledCharScaling];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_SA_INTEGER)=TC_SA_INTEGER then
    FTextCaps:=FTextCaps+[tcIntMultiScaling];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_SA_CONTIN)=TC_SA_CONTIN then
    FTextCaps:=FTextCaps+[tcAnyMultiExactScaling];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_EA_DOUBLE)=TC_EA_DOUBLE then
    FTextCaps:=FTextCaps+[tcDoubleWeightChars];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_IA_ABLE)=TC_IA_ABLE then
    FTextCaps:=FTextCaps+[tcItalics];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_UA_ABLE)=TC_UA_ABLE then
    FTextCaps:=FTextCaps+[tcUnderlines];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and  TC_SO_ABLE)=TC_SO_ABLE then
    FTextCaps:=FTextCaps+[tcStrikeouts];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_RA_ABLE)=TC_RA_ABLE then
    FTextCaps:=FTextCaps+[tcRasterFonts];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_VA_ABLE)=TC_VA_ABLE then
    FTextCaps:=FTextCaps+[tcVectorFonts];
  if (GetDeviceCaps(lDC,{$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.TEXTCAPS) and TC_SCROLLBLT)=TC_SCROLLBLT then
    FTextCaps:=FTextCaps+[tcNoScrollUsingBlts];

  if GetDeviceCaps(lDC,_SHADEBLENDCAPS)<>SB_NONE then begin
    if (GetDeviceCaps(lDC,_SHADEBLENDCAPS) and SB_CONST_ALPHA)=SB_CONST_ALPHA then
      FShadeBlendCaps:=FShadeBlendCaps+[sbcCONSTALPHA];
    if (GetDeviceCaps(lDC,_SHADEBLENDCAPS) and SB_GRAD_RECT)=SB_GRAD_RECT then
      FShadeBlendCaps:=FShadeBlendCaps+[sbcGRADRECT];
    if (GetDeviceCaps(lDC,_SHADEBLENDCAPS) and SB_GRAD_TRI)=SB_GRAD_TRI then
      FShadeBlendCaps:=FShadeBlendCaps+[sbcGRADTRI];
    if (GetDeviceCaps(lDC,_SHADEBLENDCAPS) and SB_PIXEL_ALPHA)=SB_PIXEL_ALPHA then
      FShadeBlendCaps:=FShadeBlendCaps+[sbcPIXELALPHA];
    if (GetDeviceCaps(lDC,_SHADEBLENDCAPS) and SB_PREMULT_ALPHA)=SB_PREMULT_ALPHA then
      FShadeBlendCaps:=FShadeBlendCaps+[sbcPREMULTALPHA];
  end;

  if GetDeviceCaps(lDC,_COLORMGMTCAPS)<>CM_NONE then begin
    if (GetDeviceCaps(lDC,_COLORMGMTCAPS) and CM_CMYK_COLOR)=CM_CMYK_COLOR then
      FColorMgmtCaps:=FColorMgmtCaps+[cmcCMYKColor];
    if (GetDeviceCaps(lDC,_COLORMGMTCAPS) and CM_DEVICE_ICM)=CM_DEVICE_ICM then
      FColorMgmtCaps:=FColorMgmtCaps+[cmcDeviceICM];
    if (GetDeviceCaps(lDC,_COLORMGMTCAPS) and CM_GAMMA_RAMP)=CM_GAMMA_RAMP then
      FColorMgmtCaps:=FColorMgmtCaps+[cmcGammaRamp];
  end;

  i:=0;
  while EnumDisplaySettings(nil,i,Devmode) do
    with Devmode do begin
      s:=Format('%d x %d - %d bit',[dmPelsWidth,dmPelsHeight,dmBitsPerPel]);
      if FVidModes.IndexOf(s)=-1 then
        FVidModes.Add(s);
      Inc(i);
    end;
  finally
  ReleaseDC(0, lDC);
  end;

  SetDataAvail(True);
end;

procedure GetCurveCapsStr;
begin
  with ACaps do begin
    Add(Format('Circles=%d',[integer(ccCircles in CurveCaps)]));
    Add(Format('Pie Wedges=%d',[integer(ccPieWedges in CurveCaps)]));
    Add(Format('Chords=%d',[integer(ccChords in CurveCaps)]));
    Add(Format('Ellipses=%d',[integer(ccEllipses in CurveCaps)]));
    Add(Format('Wide Borders=%d',[integer(ccWideBorders in CurveCaps)]));
    Add(Format('Styled Borders=%d',[integer(ccStyledBorders in CurveCaps)]));
    Add(Format('Wide and Styled Borders=%d',[integer(ccWideStyledBorders in CurveCaps)]));
    Add(Format('Interiors=%d',[integer(ccInteriors in CurveCaps)]));
    Add(Format('Rounded Rectangles=%d',[integer(ccRoundedRects in CurveCaps)]));
  end;
end;

procedure GetLineCapsStr;
begin
  with ACaps do begin
    Add(Format('Polylines=%d',[integer(lcPolylines in LineCaps)]));
    Add(Format('Markers=%d',[integer(lcMarkers in LineCaps)]));
    Add(Format('Multiple Markers=%d',[integer(lcMultipleMarkers in LineCaps)]));
    Add(Format('Wide Lines=%d',[integer(lcWideLines in LineCaps)]));
    Add(Format('Styled Lines=%d',[integer(lcStyledLines in LineCaps)]));
    Add(Format('Wide and Styled Lines=%d',[integer(lcWideStyledLines in LineCaps)]));
    Add(Format('Interiors=%d',[integer(lcInteriors in LineCaps)]));
  end;
end;

procedure GetPolygonCapsStr;
begin
  with ACaps do begin
    Add(Format('Alternate Fill Polygons=%d',[integer(pcAltFillPolygons in PolygonCaps)]));
    Add(Format('Rectangles=%d',[integer(pcRectangles in PolygonCaps)]));
    Add(Format('Winding Fill Polygons=%d',[integer(pcWindingFillPolygons in PolygonCaps)]));
    Add(Format('Single Scanlines=%d',[integer(pcSingleScanlines in PolygonCaps)]));
    Add(Format('Wide Borders=%d',[integer(pcWideBorders in PolygonCaps)]));
    Add(Format('Styled Borders=%d',[integer(pcStyledBorders in PolygonCaps)]));
    Add(Format('Wide and Styled Borders=%d',[integer(pcWideStyledBorders in PolygonCaps)]));
    Add(Format('Interiors=%d',[integer(pcInteriors in PolygonCaps)]));
  end;
end;

procedure GetRasterCapsStr;
begin
  with ACaps do begin
    Add(Format('Requires Banding=%d',[integer(rcRequiresBanding in RasterCaps)]));
    Add(Format('Can Transfer Bitmaps=%d',[integer(rcTransferBitmaps in RasterCaps)]));
    Add(Format('Supports Bitmaps > 64K=%d',[integer(rcBitmaps64K in RasterCaps)]));
    Add(Format('Supports SetDIBits and GetDIBits=%d',[integer(rcSetGetDIBits in RasterCaps)]));
    Add(Format('Supports SetDIBitsToDevice=%d',[integer(rcSetDIBitsToDevice in RasterCaps)]));
    Add(Format('Can Perform Floodfills=%d',[integer(rcFloodfills in RasterCaps)]));
    Add(Format('Supports Windows 2.0 Features=%d',[integer(rcWindows2xFeatures in RasterCaps)]));
    Add(Format('Palette Based=%d',[integer(rcPaletteBased in RasterCaps)]));
    Add(Format('Scaling=%d',[integer(rcScaling in RasterCaps)]));
    Add(Format('Supports StretchBlt=%d',[integer(rcStretchBlt in RasterCaps)]));
    Add(Format('Supports StretchDIBits=%d',[integer(rcStretchDIBits in RasterCaps)]));
  end;
end;

procedure GetTextCapsStr;
begin
  with ACaps do begin
    Add(Format('Capable of Character Output Precision=%d',[integer(tcCharOutPrec in TextCaps)]));
    Add(Format('Capable of Stroke Output Precision=%d',[integer(tcStrokeOutPrec in TextCaps)]));
    Add(Format('Capable of Stroke Clip Precision=%d',[integer(tcStrokeClipPrec in TextCaps)]));
    Add(Format('Supports 90 Degree Character Rotation=%d',[integer(tcCharRotation90 in TextCaps)]));
    Add(Format('Supports Character Rotation to Any Angle=%d',[integer(tcCharRotationAny in TextCaps)]));
    Add(Format('X And Y Scale Independent=%d',[integer(tcScaleIndependent in TextCaps)]));
    Add(Format('Supports Doubled Character Scaling=%d',[integer(tcDoubledCharScaling in TextCaps)]));
    Add(Format('Supports Integer Multiples Only When Scaling=%d',[integer(tcIntMultiScaling in TextCaps)]));
    Add(Format('Supports Any Multiples For Exact Character Scaling=%d',[integer(tcAnyMultiExactScaling in TextCaps)]));
    Add(Format('Supports Double Weight Characters=%d',[integer(tcDoubleWeightChars in TextCaps)]));
    Add(Format('Supports Italics=%d',[integer(tcItalics in TextCaps)]));
    Add(Format('Supports Underlines=%d',[integer(tcUnderlines in TextCaps)]));
    Add(Format('Supports Strikeouts=%d',[integer(tcStrikeouts in TextCaps)]));
    Add(Format('Supports Raster Fonts=%d',[integer(tcRasterFonts in TextCaps)]));
    Add(Format('Supports Vector Fonts=%d',[integer(tcVectorFonts in TextCaps)]));
    Add(Format('Cannot Scroll Using Blts=%d',[integer(tcNoScrollUsingBlts in TextCaps)]));
  end;
end;

procedure TMiTeC_Display.Clear;
begin
  Finalize(FAdapters);
  FVidModes.Clear;
  FColorMgmtCaps:=[];
  FShadeBlendCaps:=[];
  FTextCaps:=[];
  FRasterCaps:=[];
  FPolygonCaps:=[];
  FLineCaps:=[];
  FCurveCaps:=[];
end;

constructor TMiTeC_Display.Create;
begin
  inherited Create(AOwner);
  FVidModes:=TStringList.Create;
end;

destructor TMiTeC_Display.Destroy;
begin
  Finalize(FAdapters);
  FVidModes.Free;
  inherited;
end;

function TMiTeC_Display.GetAdapter(AIndex: Integer): TDisplayAdapter;
begin
  Result:=FAdapters[AIndex];
end;

function TMiTeC_Display.GetAdapterCount: integer;
begin
  Result:=Length(FAdapters);
end;

function TMiTeC_Display.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  n,i: Integer;
  cmc: TColorMgmtCap;
  cc: TCurveCap;
  lc: TLineCap;
  pc: TPolygonCap;
  rc: TRasterCap;
  sbc: TShadeBlendCap;
  tc: TTextCap;
  r: TDisplayAdapter;

function ReadFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
   try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            SetLength(FAdapters,Length(FAdapters)+1);
            with FAdapters[High(FAdapters)] do begin
              Name:=ReadStrProperty(sl,'Adapter');
              DAC:=ReadStrProperty(sl,'DAC');
              Chipset:=ReadStrProperty(sl,'Chipset');
              BIOS:=ReadStrProperty(sl,'BIOS');
              Memory:=ReadIntProperty(sl,'Memory');
            end;
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

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
            r.Name:=ReadStrProperty(sl,'Adapter');
            r.Chipset:=ReadStrProperty(sl,'Chipset');
            r.DAC:=ReadStrProperty(sl,'DAC');
            r.Memory:=ReadIntProperty(sl,'Memory');

            if r.Name<>'' then begin
              SetLength(FAdapters,Length(FAdapters)+1);
              FAdapters[High(FAdapters)]:=r;
            end;

            Self.FBIOSDate:=ReadStrProperty(sl,'BIOSDate');
            Self.FBIOSString:=ReadStrProperty(sl,'BIOSString');
            Self.FBIOSVersion:=ReadStrProperty(sl,'BIOSVersion');
            Self.FColorDepth:=ReadIntProperty(sl,'ColorDepth');
            n:=ReadIntProperty(sl,'ColorMgmtCaps');
            Self.FColorMgmtCaps:=[];
            for cmc:=Low(TColorMgmtCap) to High(TColorMgmtCap) do
              if n and Round(Power(2,Integer(cmc)))<>0 then
                Self.FColorMgmtCaps:=Self.FColorMgmtCaps+[cmc];
            n:=ReadIntProperty(sl,'CurveCaps');
            Self.FCurveCaps:=[];
            for cc:=Low(TCurveCap) to High(TCurveCap) do
              if n and Round(Power(2,Integer(cc)))<>0 then
                Self.FCurveCaps:=Self.FCurveCaps+[cc];
            Self.FDDV:=ReadIntProperty(sl,'DeviceDriverVersion');
            Self.FFontSize:=ReadIntProperty(sl,'FontResolution');
            Self.FHorzRes:=ReadIntProperty(sl,'HorzRes');
            Self.FVertRes:=ReadIntProperty(sl,'VertRes');
            Self.FHorzSize:=ReadIntProperty(sl,'HorzSize');
            Self.FVertSize:=ReadIntProperty(sl,'VertSize');
            n:=ReadIntProperty(sl,'LineCaps');
            Self.FLineCaps:=[];
            for lc:=Low(TLineCap) to High(TLineCap) do
              if n and Round(Power(2,Integer(lc)))<>0 then
                Self.FLineCaps:=Self.FLineCaps+[lc];
            Self.FVidModes.CommaText:=ReadStrProperty(sl,'Modes');
            Self.FPixelDiagonal:=ReadIntProperty(sl,'PixelDiagonal');
            Self.FPixelHeight:=ReadIntProperty(sl,'PixelHeight');
            Self.FPixelWidth:=ReadIntProperty(sl,'PixelWidth');
            Self.FTechnology:=ReadStrProperty(sl,'Technology');
            Self.FVRR:=ReadIntProperty(sl,'VerticalRefreshRate');
            n:=ReadIntProperty(sl,'PolygonCaps');
            Self.FPolygonCaps:=[];
            for pc:=Low(TPolygonCap) to High(TPolygonCap) do
              if n and Round(Power(2,Integer(pc)))<>0 then
                Self.FPolygonCaps:=Self.FPolygonCaps+[pc];
            n:=ReadIntProperty(sl,'RasterCaps');
            Self.FRasterCaps:=[];
            for rc:=Low(TRasterCap) to High(TRasterCap) do
              if n and Round(Power(2,Integer(rc)))<>0 then
                Self.FRasterCaps:=Self.FRasterCaps+[rc];
            n:=ReadIntProperty(sl,'TextCaps');
            Self.FTextCaps:=[];
            for tc:=Low(TTextCap) to High(TTextCap) do
              if n and Round(Power(2,Integer(tc)))<>0 then
                Self.FTextCaps:=Self.FTextCaps+[tc];
            n:=ReadIntProperty(sl,'ShadeBlendCaps');
            Self.FShadeBlendCaps:=[];
            for sbc:=Low(TShadeBlendCap) to High(TShadeBlendCap) do
              if n and Round(Power(2,Integer(sbc)))<>0 then
                Self.FShadeBlendCaps:=Self.FShadeBlendCaps+[sbc];

            i:=0;
            while ReadFromStream(i) do
              Inc(i);

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

procedure TMiTeC_Display.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  n,i: Integer;
  cmc: TColorMgmtCap;
  cc: TCurveCap;
  lc: TLineCap;
  pc: TPolygonCap;
  rc: TRasterCap;
  sbc: TShadeBlendCap;
  tc: TTextCap;

  procedure WriteToStream(AIndex: Integer);
  var
    strm: TStorageStream;
    sl: TStringList;
  begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Adapter',Self.FAdapters[AIndex].Name);
      WriteStrProperty(sl,'DAC',Self.FAdapters[AIndex].DAC);
      WriteStrProperty(sl,'Chipset',Self.FAdapters[AIndex].Chipset);
      WriteStrProperty(sl,'BIOS',Self.FAdapters[AIndex].BIOS);
      WriteIntProperty(sl,'Memory',Self.FAdapters[AIndex].Memory);
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
      try
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      sl.Free;
    end;
  end;

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
      for i:=0 to High(Self.FAdapters) do
        WriteToStream(i);

      sl:=TStringList.Create;
      try
         WriteStrProperty(sl,'BIOSString',Self.BIOSString);
         WriteStrProperty(sl,'BIOSDate',Self.BIOSDate);
         WriteStrProperty(sl,'BIOSVersion',Self.BIOSVersion);
         WriteStrProperty(sl,'Technology',Self.Technology);
         WriteIntProperty(sl,'ColorDepth',Self.ColorDepth);
         WriteIntProperty(sl,'HorzRes',Self.HorzRes);
         WriteIntProperty(sl,'VertRes',Self.VertRes);
         WriteIntProperty(sl,'HorzSize',Self.HorzSize);
         WriteIntProperty(sl,'VertSize',Self.VertSize);
         WriteStrProperty(sl,'Modes',Self.Modes.CommaText);
         WriteIntProperty(sl,'FontResolution',Self.FontResolution);
         WriteIntProperty(sl,'PixelDiagonal',Self.PixelDiagonal);
         WriteIntProperty(sl,'PixelWidth',Self.PixelWidth);
         WriteIntProperty(sl,'PixelHeight',Self.PixelHeight);
         WriteIntProperty(sl,'VerticalRefreshRate',Self.VerticalRefreshRate);
         WriteIntProperty(sl,'DeviceDriverVersion',Self.DeviceDriverVersion);
         n:=0;
         for tc:=Low(TTextCap) to High(TTextCap) do
           if tc in Self.TextCaps then
             n:=n+Round(Power(2,Integer(tc)));
         WriteIntProperty(sl,'TextCaps',n);
         n:=0;
         for lc:=Low(TLineCap) to High(TLineCap) do
           if lc in Self.LineCaps then
             n:=n+Round(Power(2,Integer(lc)));
         WriteIntProperty(sl,'LineCaps',n);
         n:=0;
         for rc:=Low(TRasterCap) to High(TRasterCap) do
           if rc in Self.RasterCaps then
             n:=n+Round(Power(2,Integer(rc)));
         WriteIntProperty(sl,'RasterCaps',n);
         n:=0;
         for cc:=Low(TCurveCap) to High(TCurveCap) do
           if cc in Self.CurveCaps then
             n:=n+Round(Power(2,Integer(cc)));
         WriteIntProperty(sl,'CurveCaps',n);
         n:=0;
         for pc:=Low(TPolygonCap) to High(TPolygonCap) do
           if pc in Self.PolygonCaps then
             n:=n+Round(Power(2,Integer(pc)));
         WriteIntProperty(sl,'PolygonCaps',n);
         n:=0;
         for cmc:=Low(TColorMgmtCap) to High(TColorMgmtCap) do
           if cmc in Self.ColorMgmtCaps then
             n:=n+Round(Power(2,Integer(cmc)));
         WriteIntProperty(sl,'ColorMgmtCaps',n);
         n:=0;
         for sbc:=Low(TShadeBlendCap) to High(TShadeBlendCap) do
           if sbc in Self.ShadeBlendCaps then
             n:=n+Round(Power(2,Integer(sbc)));
         WriteIntProperty(sl,'ShadeBlendCaps',n);

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

procedure GetColorMgmtCapsStr;
begin
  with ACaps do begin
    Add(Format('CMYK color space ICC color profile=%d',[integer(cmcCMYKColor in ColorMgmtCaps)]));
    Add(Format('ICM performation=%d',[integer(cmcDeviceICM in ColorMgmtCaps)]));
    Add(Format('Gamma Ramp support=%d',[integer(cmcGammaRamp in ColorMgmtCaps)]));
  end;
end;

procedure GetShadeBlendCapsStr;
begin
  with ACaps do begin
    Add(Format('Source Constant Alpha handling=%d',[integer(sbcConstAlpha in ShadeBlendCaps)]));
    Add(Format('GradientFill rectangles=%d',[integer(sbcGradRect in ShadeBlendCaps)]));
    Add(Format('GradientFill triangles=%d',[integer(sbcGradTri in ShadeBlendCaps)]));
    Add(Format('Per-pixel alpha handling=%d',[integer(sbcPixelAlpha in ShadeBlendCaps)]));
    Add(Format('Premultiplied alpha handling=%d',[integer(sbcPremultAlpha in ShadeBlendCaps)]));
  end;
end;

end.
