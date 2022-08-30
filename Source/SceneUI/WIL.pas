unit WIL;

interface

uses
  Windows, Classes, Graphics, SysUtils, Generics.Collections, Generics.Defaults,
  PXL.Devices, PXL.Textures, PXL.Canvas, PXL.Providers, ZLib, IOUtils, uEDCode, uGameEngine,
  uCommon, Types, pngimage, Dialogs, uMiniResFileInfo;

const
  CHECK_FREE_LIB_TIME = 10 * 1000 ; //  每个图库文件检查释放的单个时间。
  FREE_TEXTURE_TIME =  20 * 1000; //多久没使用的纹理将会被释放。
  CHECK_FREE_TEXTURE_INTERVAL = 5 * 1000; //多久检查释放一下纹理

type
  TMiniState = (msNormal,msRequested);
  // 节省内存的 PartInfo类。 给客户端使用 客户端只要知道
  // PartCount 和  每个Image 对应的PartCount

  PTImageFilePartInfoLite = ^TImageFilePartInfoLite;
  TImageFilePartInfoLite = packed record
  private
    FPartIndex : Array of Word; //对应的图片序号 所对应的 分割部分。
    FPartRequestTick:Array of Cardinal; //每个Part 请求的时间
  public
    function GetPartCount:Integer;
  end;

  TDXImage = packed record
    nPx:SmallInt;
    nPy:SmallInt;
    Surface: TCustomLockableTexture;
    dwLatestTime:LongWord;
  end;
  pTDxImage = ^TDXImage;

  TWMImages = class
  private
    FFileName: String;
    Device:TCustomDevice;
    FImageCount: integer;
    m_dwMemChecktTick: LongWord;
    FReadTime: LongWord;
    FHasData: Boolean;
    FInitialize: Boolean;
    m_ImgArr: array of pTDxImage;
    m_IndexList: TList<Integer>;
    m_FileStream: TFileStream;
    FVT: Boolean;
    FAppr: Word;
    m_boMiniCreate:Boolean; //标志此文件是通过微端建立的。
    m_PartInfo : PTImageFilePartInfoLite;
    m_boImportantDown:Boolean;
    procedure SetFileName(const Value: String);
    procedure FreeOldMemorys;
    function GetEmpty: Boolean;
    function GetIndex(index:Integer):Integer;
    function GetPartByIndex(Index:Integer):Integer;
    function GetLastRequestTime(Index:Integer):Cardinal; //获取对应图片上次请求的时间。
    procedure SetRequestTime(index:Integer; Time:Cardinal);  //设置请求时间
  protected
    procedure LoadDxImage(Index, position: integer; pdximg: PTDxImage); virtual;
    function GetImageSurface(index: integer): TCustomLockableTexture;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight: Integer); overload; virtual;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer); overload; virtual;
  public
    class var AllWMImages : TList<TWMImages>;
    class var MemmoryUsage:Int64;
    class function ResourceGetMem(Size:Cardinal):Pointer; // 申请内存 用于统计资源 使用的内存数量
    class procedure ResourceFreeMem(P:Pointer;Size:Cardinal);// 释放内存。
    class procedure AddMemSize(Size:Cardinal);
    class procedure DecMemSize(Size:Cardinal);
  public
    class procedure CreateNewFile(const FileName: String; BitCount: Integer); virtual;
    constructor Create(ADevice:TCustomDevice); virtual;
    destructor Destroy;override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure ClearCache;
    procedure ReLoad();virtual;
    //获取图片的数据大小方便对其进行拆分分割。
    function GetImgDataSize(Index:Integer):Integer;virtual;
    function GetCachedImage(index: integer; var m_wPx, m_wPy: integer): TCustomLockableTexture;
    function GetImgSize(index: Integer; var AWidth, AHeight: Integer): Boolean; overload;
    function GetImgSize(index: Integer; var AWidth, AHeight, m_wPx, m_wPy: Integer): Boolean; overload;
    procedure Extract(Index: Integer; var AWidth, AHeight, DataLen: Integer; var Data: PAnsiChar); virtual;
    property Important : Boolean read m_boImportantDown write m_boImportantDown;
    property FileName: String read FFileName write SetFileName;
    property ImageCount: integer read FImageCount;
    property Images[index: integer]: TCustomLockableTexture read GetImageSurface;
    property VT: Boolean read FVT;
    property Empty: Boolean read GetEmpty;
    property ReadTime: LongWord read FReadTime;
    property FileStream:TFileStream read m_FileStream;
    property Index[Index:Integer]:Integer read GetIndex;
    property MiniCreate:Boolean read m_boMiniCreate write m_boMiniCreate;
    property Appr: Word read FAppr write FAppr;
  end;

  TWMVirtualImages = class(TWMImages)
  protected
    procedure LoadDxImage(Index, position: integer; pdximg: PTDxImage); override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight: Integer); override;
  public
    constructor Create(ADevice:TCustomDevice); override;
    procedure Initialize; override;
  end;

  TOnAddDownloadImage = procedure(const FileName: String; ImageIndex: Integer ; Important:Boolean) of Object;
  TOnReloadFinished = procedure(const FileName: String) of Object;
  TWMImageLibManager = class
  private
    FLibs,
    FMiniUseLibs, //微端客户端会使用到 仅仅为微端客户端使用
    FFullNameLibs: TDictionary<String, TWMImages>;
    FMiniOpend: Boolean;
    FAddDownloadImage: TOnAddDownloadImage;
    FReloadFinished:TOnReloadFinished;
    FMiniImageFile:TDictionary<String, String>;
    FPartInfo: TDictionary<String,PTImageFilePartInfoLite>;
    FPassWordList:TDictionary<String,String>; //密码hash表
    procedure SetMiniOpen(Value:Boolean);
    constructor Create;
    destructor Destroy; override;
    procedure AddLib(const AName: String; Lib: TWMImages);
  public
    function TryGetLibByFullName(const AName: String; out Lib: TWMImages): Boolean;
    //获取图片对象 图片对象不存在则创建
    function TryGetLib(const AName: String; out Lib: TWMImages): Boolean;
    function TryGetLib2(const AName: String; out Lib: TWMImages): Boolean;
    //获取图片对象 图片对象不存在也不创建
    function TryGetLibSatic(const AName: String; out Lib: TWMImages): Boolean;
    procedure LoadLibNames(ANames: TStrings);
    procedure LoadImage(const FileName: String; Index, Position: Integer);
    procedure FreeMemory;
    procedure ClearTextures;
    procedure ReLoadImageFile(const FileName:String);
    procedure AddDownloadImage(const FileName: String; ImageIndex: Integer ; Important:Boolean);
    procedure FileReLoadFinish(const FileName:String);
    function LoadMiniImageFile(const FileName:String):Boolean;
    function LoadPartInfoFile(const Value:TImageFilePartInfo):Integer;
    function GetMiniFileName(const FileName:String):String; //根据微端配置的文件获取到微端的包含文件。
    procedure AddPassWord(const FileName,PassWord:String); //添加一个密码配置文件
    procedure FreeAllTextureMemory(); //清除所有纹理内存。 用于测试
    property MiniOpend: Boolean read FMiniOpend write SetMiniOpen;
    property OnAddDownloadImage: TOnAddDownloadImage read FAddDownloadImage write FAddDownloadImage;
    property OnReLoadFinished:TOnReloadFinished read FReloadFinished write FReloadFinished;
  end;

type
  TWMImageHeader = record
    Title: String[40];
    ImageCount: integer;
    ColorCount: integer;
    PaletteSize: integer;
    VerFlag:integer;
  end;

  TWMIndexHeader = record
    Title: string[40];
    IndexCount: integer;
    VerFlag:integer;
  end;

  TWMImageInfo = record
    m_nWidth    :Word;
    m_nHeight   :Word;
    m_wPx: SmallInt;
    m_wPy: SmallInt;
    ImageVerSion: LongWord;
    nSize: LongWord;
  end;

  PRGBQuads = ^TRGBQuads;
  TRGBQuads = array[0..255] of TRGBQuad;
  TWILImages = class(TWMImages)
  private
    BytesPerPixe,
    FBitCount: Byte;
    btVersion: Byte;
    procedure LoadPalette;
  protected
    procedure LoadIndex(const FileName: string);
    procedure LoadDxImage(Index, position: integer; pdximg: PTDxImage); override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight: Integer); overload; override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer); overload; override;
  public
    procedure Initialize; override;
    class procedure CreateNewFile(const FileName: String; BitCount: Integer); override;
  end;

  TWZXIndexHeader = record
    Title: string[43];
    IndexCount: Integer;
  end;
  PWZXIndexHeader = ^TWZXIndexHeader;

  TWZLImageHeader = record
    Title: string[43];
    IndexCount: Integer;
    Reserved: array[0..15] of Byte;
  end;
  PWZLImageHeader = ^TWZLImageHeader;

  TWZLImageInfo = packed record //16
    m_Enc1: Byte; //1 3:8位  5:16位 7：24位  9 :32位 兼容GOM 随云
    m_Enc2: Byte; // = 9表示 有透明通道
    m_type1: Byte; //1 不清楚
    m_type2: Byte; //1 不清楚0
    m_nWidth: SmallInt; //2 宽度
    m_nHeight: SmallInt; //2 高度
    m_wPx: SmallInt; //2 x
    m_wPy: SmallInt; //2 y
    m_Len: Cardinal; //4 压缩数据长度
  end;
  PWZLImageInfo = ^TWZLImageInfo;

  TWZLImages = class(TWMImages)
  protected
    procedure LoadIndex(const FileName: string);
    procedure LoadDxImage(Index, position: integer; pdximg: PTDxImage); override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight: Integer); overload; override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer); overload; override;
  public
    procedure Initialize; override;
    procedure Reload;override;
    function GetImgDataSize(Index:Integer):Integer;override;
    procedure Extract(Index: Integer; var AWidth, AHeight, DataLen: Integer; var Data: PAnsiChar); override;
  end;

  TPackDataHeader = record //新定义的Data文件头
    Title: string[40];
    ImageCount: Integer;
    IndexOffSet: Integer;
    XVersion: Word;
    Password: String[16];
  end;
  pTPackDataHeader = ^TPackDataHeader;

  TPackDataImageInfo = packed record
    m_nWidth: Word;
    m_nHeight: Word;
    bitCount: Byte;
    m_wPx: SmallInt;
    m_wPy: SmallInt;
    m_Len: LongWord; //数据大小
    GraphicType: TGraphicType; //0:DIB,BMP 1:PNG
  end;
  pTPackDataImageInfo = ^TPackDataImageInfo;

  TWMPackageImages = class(TWMImages)
  private
    const _HKey = '7BB2FA4F-2A6A-4632-B72F-F98D440E8C36';
    const _Key = 'CFBA39C1-72A6-4171-9FF0-CF1920DD76F3';
  private
    FDataHeader: TPackDataHeader;
    FPassWord: AnsiString;
    m_SortedIndexPosition : TList<Integer>;
    m_boSortedIndex:Boolean;
    m_PassWord : String;
    function ReadHeader: TPackDataHeader;
    function ReadImageInfo(APosition: Integer): TPackDataImageInfo;
    function CheckImageInfoSize(APosition: Integer): Boolean;
    function GetIsEncrypt():Boolean;
    procedure SortIndex();
  protected
    procedure LoadIndex;
    procedure LoadDxImage(Index, position: integer; pdximg: PTDxImage); override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight: Integer); override;
    procedure DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer); overload; override;
  public
    class procedure CreateNewFile(const FileName: String; BitCount: Integer); override;
    class procedure WriteHeader(AStream: TStream; AHeader: TPackDataHeader);
    class function ReadFileHeader(AStream:TStream):TPackDataHeader;
    procedure Initialize; override;
    function GetImgDataSize(Index:Integer):Integer; override;
    procedure Reload();override;
    property IsEncrypt:Boolean read GetIsEncrypt; //文件是否加密。
    property PassWord:String read m_PassWord write m_PassWord;
  end;

const
  WMImageInfoSize: Integer = 16;//TWMImageInfo;
  WZLImageInfoSize: Integer = 16;//TWZLImageInfo
  PackDataImageInfoSize: Integer = 14;//TPackDataImageInfo

var
  MIRPath: String = '';
  ResDir: String = '91Resource\';
  LibManager: TWMImageLibManager;
  ExceptionOut : Procedure(const Msg:String);
  ResourceInitOK:Boolean = false;


function CreateImages(const AFileName: String; ADevice:TCustomDevice; ISFixedFile: Boolean=False): TWMImages;
function CreateImageFile(const AFileName: String; BitCount: Integer; ADevice:TCustomDevice): TWMImages;
//function CreateImages2(const AFileName: String; ISFixedFile: Boolean): TWMImages;
function CreateImages2(const AFileName: String; ISFixedFile: Boolean; ADevice:TCustomDevice): TWMImages;
function FindWLib(const AName: String): TWMImages;
procedure GetClientLibFile(const ResourceDir : string); //获取客户端所有资源文件。
//额外包装一个不会报异常的函数 随云
function CreateFileStream(const FileName:string ; Mode:Word) : TFileStream;
procedure ConsoleDebug(const Log:String);implementation

var
  ClMain, ClientFileHash: TDictionary<string, Boolean>;

procedure ConsoleDebug(const Log:String);
begin
  {$IFDEF CONSOLE}
  Writeln(Log);
  {$ENDIF}
end;

function MyFileExists(FileName:String):Boolean;
begin
  {$IFDEF QUICKFILELOAD}
//  if ResourceInitOK then begin
//    Result:= FileExists(FileName);
//  end else begin
    Result := False;
    ClientFileHash.TryGetValue(UpperCase(FileName),Result);
//  end;
  {$ELSE}
  Result:= FileExists(FileName)
  {$ENDIF}
end;

function CreateFileStream(const FileName : String; Mode:Word) : TFileStream;
begin
  Result := nil;
  if FileExists(FileName) then
  begin
    Try
      if Pos('Prguse',FileName) > 0 then
        Result := nil;
      Result := TFileStream.Create(FileName,Mode);
    Except
      on E :Exception do
      begin
        if Assigned(ExceptionOut) then
        begin
          ExceptionOut('无法访问文件 : ' + FileName + ' 。该文件可能被其他程序占用 或者被禁止读取！')
        end;
      end;
    End;
  end;
end;

function TrySearchFileName(const AFileName, AExt: String; var ANewFileName: String): Boolean;
var
  S: String;
begin
  Result := False;
  S := AFileName;
  if AExt <> '' then
    S := ChangeFileExt(S, AExt);

  if MyFileExists(MIRPath + S) then
  begin
    ANewFileName := MIRPath + S;
    Result := True;
    Exit;
  end;
  if MyFileExists(MIRPath + 'Data\' + S) then
  begin
    ANewFileName := MIRPath + 'Data\' + S;
    Result := True;
    Exit;
  end;

  if MyFileExists(ResDir + 'Data\' + S) then
  begin
    ANewFileName := ResDir + 'Data\' + S;
    Result := True;
    Exit;
  end;

  if MyFileExists(MIRPath + ResDir + S) then
  begin
    ANewFileName := MIRPath + ResDir + S;
    Result := True;
    Exit;
  end;
  if MyFileExists(MIRPath + ResDir + 'Data\' + S) then
  begin
    ANewFileName := MIRPath + ResDir + 'Data\' + S;
    Result := True;
    Exit;
  end;
end;

function MyTrySearchFileName(const AFileName, AExt: String; var ANewFileName: String): Boolean;
var
  S: String;
begin
  Result := False;
  S := AFileName;
  if AExt <> '' then
    S := ChangeFileExt(S, AExt);
  if MyFileExists(MIRPath + S) then
  begin
    ANewFileName := MIRPath + S;
    Result := True;
    Exit;
  end;

  if MyFileExists(MIRPath + 'Data\' + S) then
  begin
    ANewFileName := MIRPath + 'Data\' + S;
    Result := True;
    Exit;
  end;
end;

function MyTrySearchFileNameResDir(const AFileName, AExt: String; var ANewFileName: String): Boolean;
var
  S: String;
begin
  Result := False;
  S := AFileName;
  if AExt <> '' then
    S := ChangeFileExt(S, AExt);
  if MyFileExists(MIRPath + ResDir + 'Data\' + S) then
  begin
    ANewFileName := MIRPath + ResDir + 'Data\' + S;
    Result := True;
    Exit;
  end;
end;

function NameExpandToFileName(const AName: String): String;
begin
  Result := StringReplace(AName, '/', '\', [rfReplaceAll]);
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
  Result := Trim(Result);
  if Result <> '' then
  begin
    if AName[1] = '$' then
    begin
      Delete(Result, 1, 1);
      Result := MIRPath + ResDir + Result;
      Exit;
    end;

    if FileExists(Result) then
      Exit;

    //先根据写死的目录文件找
    if TrySearchFileName(Result, '', Result) then
      Exit;
    if TrySearchFileName(Result, '.wzl', Result) then
      Exit;
    if TrySearchFileName(Result, '.wil', Result) then
      Exit;
    if TrySearchFileName(Result, '.wis', Result) then
      Exit;
    if TrySearchFileName(Result, '.data', Result) then
      Exit;

//    //如果没找到 再去找91资源目录找
//    if MyTrySearchFileNameResDir(Result, '', Result) then
//      Exit;
//    if MyTrySearchFileNameResDir(Result, '.wzl', Result) then
//      Exit;
//    if MyTrySearchFileNameResDir(Result, '.wil', Result) then
//      Exit;
//    if MyTrySearchFileNameResDir(Result, '.wis', Result) then
//      Exit;
//    if MyTrySearchFileNameResDir(Result, '.data', Result) then
//      Exit;
  end;
end;

function CreateImages2(const AFileName: String; ISFixedFile: Boolean; ADevice:TCustomDevice): TWMImages;
var
  Ext, TmpFileName: String;
begin
  Result := nil;
  if ISFixedFile then
  begin
    TmpFileName := AFileName;
    Ext := UpperCase(ExtractFileExt(AFileName));
  end
  else
  begin
    TmpFileName := AFileName;
    TmpFileName := ChangeFileExt(TmpFileName, '.wzl');
    if not FileExists(TmpFileName) then
    begin
      TmpFileName := ChangeFileExt(TmpFileName, '.wil');
      if not FileExists(TmpFileName) then
      begin
        TmpFileName := ChangeFileExt(TmpFileName, '.wis');
        if not FileExists(TmpFileName) then
          TmpFileName := ChangeFileExt(TmpFileName, '.data');
      end;
    end;
    Ext := UpperCase(ExtractFileExt(TmpFileName));
  end;
  if not FileExists(TmpFileName) then
    Exit;

  if SameText(Ext, '.DATA') then
    Result := TWMPackageImages.Create(ADevice)
  else if SameText(Ext, '.WIL') or SameText(Ext, '.WIS') then
    Result := TWILImages.Create(ADevice)
  else if SameText(Ext, '.WZL') then
    Result := TWZLImages.Create(ADevice);

  if Result <> nil then
    Result.FileName := TmpFileName;
end;

//function CreateImages(const AFileName: String; ISFixedFile: Boolean): TWMImages;
//var
//  Ext, TmpFileName: String;
//begin
//  Result :=  nil;
//  TmpFileName := NameExpandToFileName(AFileName);
//  Ext := UpperCase(ExtractFileExt(TmpFileName));
//  if (TmpFileName <> '') and FileExists(TmpFileName) then
//  begin
//    if SameText(Ext, '.WIL') or SameText(Ext, '.WIS') then
//      Result  :=  TWILImages.Create
//    else if SameText(Ext, '.WZL') then
//      Result  :=  TWZLImages.Create
//    else if SameText(Ext, '.DATA') then
//      Result  :=  TWMPackageImages.Create;
//    if Result <> nil then
//      Result.FileName :=  TmpFileName;
//  end;
//  if Result = nil then
//  begin
//    Result := TWMVirtualImages.Create;
//    Result.FileName := TmpFileName;
//  end;
//end;

function CreateImages(const AFileName: String; ADevice:TCustomDevice; ISFixedFile: Boolean): TWMImages;
var
  Ext, TmpFileName,MiniFileName: String;
begin

//  if ResourceInitOK then
//  begin
//    writeln('初始化游戏后, :' + AFileName);
//  end;

  Result :=  nil;
  if LibManager.MiniOpend then  //如果微端开启了 那么先获取微端上这个名字的格式。
  begin
    MiniFileName := LibManager.GetMiniFileName(AFileName);
    if MiniFileName <> '' then
    begin
      if not LibManager.TryGetLibSatic(AFileName,Result) then
      begin
        Ext := UpperCase(ExtractFileExt(MiniFileName));
        if SameText(Ext, '.WZL') then
          Result  :=  TWZLImages.Create(ADevice)
        else if SameText(Ext, '.DATA') then
        Result  :=  TWMPackageImages.Create(ADevice);
        Result.FileName := MiniFileName;
        Result.MiniCreate := True;
      end;
      exit;
    end;
  end;

  if not LibManager.TryGetLibSatic(AFileName,Result) then
  begin
    TmpFileName := NameExpandToFileName(AFileName);
    Ext := UpperCase(ExtractFileExt(TmpFileName));
    if (TmpFileName <> '') and MyFileExists(TmpFileName) then
    begin
      if SameText(Ext, '.DATA') then
        Result  :=  TWMPackageImages.Create(ADevice)
      else if SameText(Ext, '.WIL') or SameText(Ext, '.WIS') then
        Result  :=  TWILImages.Create(ADevice)
      else if SameText(Ext, '.WZL') then
        Result  :=  TWZLImages.Create(ADevice);

      if Result <> nil then
        Result.FileName :=  TmpFileName;
    end;

    if Result = nil then
    begin
      Result := TWMVirtualImages.Create(ADevice);
      Result.FileName := TmpFileName;
    end;
  end;
end;

function CreateImageFile(const AFileName: String; BitCount: Integer; ADevice:TCustomDevice): TWMImages;
var
  AExt: String;
begin
  AExt  :=  UpperCase(ExtractFileExt(AFileName));
  if SameText(AExt, '.DATA') then
    TWMPackageImages.CreateNewFile(AFileName, BitCount)
  else if SameText(AExt, '.WIL') or SameText(AExt, '.WIS') then
    Result  :=  TWILImages.Create(ADevice)
  else if SameText(AExt, '.WZL') then
    Result  :=  TWZLImages.Create(ADevice);
end;

function IsMirDataFile(const Path: string;const SearchRec: TSearchRec): Boolean;
var
  Ext:string;
begin
  Ext := UpperCase(ExtractFileExt(SearchRec.Name));
  if (Ext = '.WZL') or (Ext = '.WIL') or (Ext = '.DATA') then
  begin
    Result := True;
  end else
  begin
    Result := False;
  end;
end;

procedure GetClientLibFile(const ResourceDir : string); //获取客户端所有资源文件。
var
  FileList : TStringDynArray;
  FileDir : string;
  nLen : Integer;
  I : Integer;
  FileName : string;
begin
  ClientFileHash := TDictionary<string,Boolean>.Create(512);

  FileDir := ExtractFilePath(ParamStr(0)) + 'Data';
  nLen := Length(ExtractFilePath(ParamStr(0))) + 1;
  FileList := IOUtils.TDirectory.GetFiles(FileDir,TSearchOption.soAllDirectories,IsMirDataFile);
  for i := 0 to High(FileList) do
  begin
    FileName := UpperCase(Copy(FileList[i],nLen,255));
    ClientFileHash.Add(FileName,True);
  end;

  FileDir := ExtractFilePath(ParamStr(0)) + ResourceDir;
  FileList := IOUtils.TDirectory.GetFiles(FileDir,TSearchOption.soAllDirectories,IsMirDataFile);
  for i := 0 to High(FileList) do
  begin
    FileName := UpperCase(Copy(FileList[i],nLen,255));
    ClientFileHash.Add(FileName,True);
  end;
end;

function FindWLib(const AName: String): TWMImages;
begin
  Result := nil;
  if AName <> '' then
    LibManager.TryGetLib(AName, Result);
end;

function ExtractFileNameOnly(const fname: string): string;
var
  extpos: Integer;
  ext, fn: string;
begin
  ext := ExtractFileExt(fname);
  fn := ExtractFileName(fname);
  if ext <> '' then begin
    extpos := Pos(ext, fn);
    Result := Copy(fn, 1, extpos - 1);
  end else
    Result := fn;
end;

procedure WidthBytes(AWidth, ABitCount: Integer; var ASize: Integer); inline;
begin
  ASize := (((AWidth * ABitCount) + 31) shr 5) * 4;
end;

procedure TWILImages.DoGetImgSize(position: integer; var AWidth,
  AHeight: Integer);
var
  imginfo: TWMImageInfo;
begin
  AWidth := 0;
  AHeight := 0;
  if position + SizeOf(TWMImageInfo) <= m_FileStream.Size then
  begin
    m_FileStream.Seek(position, 0);
    m_FileStream.Read(imginfo, sizeof(TWMImageInfo) - 4);
    AWidth  :=  imginfo.m_nWidth;
    AHeight :=  imginfo.m_nHeight;
  end;
end;

class procedure TWILImages.CreateNewFile(const FileName: String;
  BitCount: Integer);
var
  AHeader: TWMIndexHeader;
  ATmpFileName: String;
  AFile: TFileStream;
begin
  ATmpFileName        :=  SysUtils.ChangeFileExt(FileName, '.wix');
  AHeader.Title       :=  '91网络资源文件';
  AHeader.IndexCount  :=  0;
  AHeader.VerFlag     :=  0;
  AFile :=  TFileStream.Create(ATmpFileName, fmCreate);
  try
    AFile.Write(AHeader, SizeOf(TWMIndexHeader));
  finally
    AFile.Free;
  end;
  ATmpFileName        :=  SysUtils.ChangeFileExt(FileName, '.wil');
  AFile :=  TFileStream.Create(ATmpFileName, fmCreate);
  AFile.Free;
end;

procedure TWILImages.DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer);
var
  imginfo: TWMImageInfo;
begin
  AWidth := 0;
  AHeight := 0;
  m_wPx := 0;
  m_wPy := 0;
  if position + SizeOf(TWMImageInfo) <= m_FileStream.Size then
  begin
    m_FileStream.Seek(position, 0);
    m_FileStream.Read(imginfo, sizeof(TWMImageInfo) - 4);
    AWidth  :=  imginfo.m_nWidth;
    AHeight :=  imginfo.m_nHeight;
    m_wPx  :=  imginfo.m_wPx;
    m_wPy  :=  imginfo.m_wPy;
  end;
end;

procedure TWILImages.Initialize;
var
  Idxfile: string;
  Header: TWMImageHeader;
begin
  inherited;
  // - 1表示没有初始化
  if ImageCount = - 1 then
  begin
    //m_FileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    //之前的如果报异常 会导致堆栈上溯使得如果程序在初始化的情况下会挂掉。
    m_FileStream := CreateFileStream(FFileName,fmOpenRead or fmShareDenyNone);
    if m_FileStream = nil then
      exit;

    if UpperCase(ExtractFileExt(FFileName))='.WIS' then
    begin
      FImageCount :=  300000;
      FBitCount := 8;
      idxfile :=  '';
    end
    else
    begin
      m_FileStream.Read(Header, SizeOf(TWMImageHeader));
      FImageCount := Header.ImageCount;
      if header.VerFlag = 0 then
      begin
        btVersion := 1;
        m_FileStream.Seek(-4, soFromCurrent);
      end else if Header.VerFlag = $20 then  begin
        btVersion := 2;
      end;
      if Header.ColorCount = 256 then
      begin
        FBitCount := 8;
        BytesPerPixe := 1;
      end
      else if header.ColorCount = 65536 then
      begin
        FBitCount := 16;
        BytesPerPixe := 2;
      end
      else if header.colorcount = 16777216 then
      begin
        FBitCount := 24;
        BytesPerPixe := 4;
      end
      else if header.ColorCount > 16777216 then
      begin
        FBitCount := 32;
        BytesPerPixe := 4;
      end;
      if Header.VerFlag = $20 then begin  //韩国版本
        FBitCount:=16;
        BytesPerPixe := 2;
      end;
      idxfile := ExtractFilePath(FFileName) + ExtractFileNameOnly(FFileName) + '.WIX';
    end;

    //m_ImgArr := ResourceGetMem(SizeOf(TDxImage) * FImageCount);
    SetLength(m_ImgArr,FImageCount);
    AddMemSize(SizeOf(pTDxImage) * FImageCount);
    if Header.VerFlag <> $20 then LoadPalette;
    LoadIndex(idxfile);
    FInitialize := True;
  end else
  begin
    ConsoleDebug('WIL 图库资源文件重复初始化:' + FFileName);
  end;
end;

procedure TWILImages.LoadIndex(const FileName: string);
var
  I, Value: integer;
  IdxHeader: TWMIndexHeader;
  F: TFileStream;
  Buf: PAnsiChar;
  BufSize: Integer;
begin
  m_IndexList.Clear;
  if FileExists(FileName) then
  begin
    //F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    F := CreateFileStream(FileName, fmOpenRead or fmShareDenyNone);

    if F = nil then
      exit;

    try
      if btVersion = 1 then
        F.ReadBuffer(IdxHeader, sizeof(TWMIndexHeader) - 4)
      else
        F.ReadBuffer(IdxHeader, sizeof(TWMIndexHeader));

      BufSize := F.Size - F.Position;
      GetMem(Buf, BufSize);
      try
        F.ReadBuffer(Buf^, BufSize);
        for I := 0 to FImageCount - 1 do
        begin
          if I * 4 < BufSize then
            Move(Buf[I * 4], Value, 4)
          else
            Value := 0;
          m_IndexList.Add(Value);
        end;
      finally
        FreeMem(Buf);
      end;
    finally
      F.Free;
    end;
  end;
end;

procedure TWILImages.LoadPalette;
begin
end;

procedure TWILImages.LoadDxImage(Index, position: integer; pdximg: PTDxImage);
var
  AImgInfo: TWMImageInfo;
  EncryptBuffer: PAnsiChar;
  ABuffer: Pointer;
  ASize: Integer;
  Compressed: Byte;
begin
  if position + SizeOf(TWMImageInfo) <= m_FileStream.Size then
  begin
    m_FileStream.Seek(position, 0);
    if btVersion = 2 then m_FileStream.Read(AImgInfo, SizeOf(TWMImageInfo))
    else if btVersion = 0 then m_FileStream.Read(AImgInfo, SizeOf(TWMImageInfo)-4)
    else m_FileStream.Read(AImgInfo, SizeOf(TWMImageInfo) -8);

    pdximg.surface := PXL.Providers.TGraphicsDeviceProvider(Device.Provider).CreateLockableTexture(Device, False);

    pdximg.nPx := AImgInfo.m_wPx;
    pdximg.nPy := AImgInfo.m_wPy;
    WidthBytes(AImgInfo.m_nWidth, FBitCount, ASize);
    ASize := ASize * AImgInfo.m_nHeight;

   if ASize + m_FileStream.Position <= m_FileStream.Size then
      begin
        GetMem(ABuffer, ASize);
        try
          m_FileStream.Read(ABuffer^, ASize);
          pdximg.surface.LoadFromDataEx(ABuffer, ASize, BytesPerPixe * 8, AImgInfo.m_nWidth, AImgInfo.m_nHeight, False, True);
        finally
          FreeMem(ABuffer);
        end;
      end;
    end;
end;

//procedure TWILImages.LoadDxImage(Index, position: integer; pdximg: PTDxImage);
//var
//  AImgInfo: TWMImageInfo;
//  EncryptBuffer: PAnsiChar;
//  ABuffer: Pointer;
//  ASize: Integer;
//  Compressed: Byte;
//begin
//  if position + SizeOf(TWMImageInfo) <= m_FileStream.Size then
//  begin
//    m_FileStream.Seek(position, 0);
//    if btVersion = 2 then m_FileStream.Read(AImgInfo, SizeOf(TWMImageInfo))
//    else if btVersion = 0 then m_FileStream.Read(AImgInfo, SizeOf(TWMImageInfo)-4)
//    else m_FileStream.Read(AImgInfo, SizeOf(TWMImageInfo) -8);
//
//    pdximg.surface := Factory.CreateLockableTexture;
//    pdximg.nPx := AImgInfo.m_wPx;
//    pdximg.nPy := AImgInfo.m_wPy;
//    WidthBytes(AImgInfo.m_nWidth, FBitCount, ASize);
//    ASize := ASize * AImgInfo.m_nHeight;
//
//    if btVersion = 2 then begin
//      GetMem(EncryptBuffer, AImgInfo.nSize);
//      m_FileStream.Read(Compressed, 1);
//      m_FileStream.Read(EncryptBuffer^, 5);
//      if byte(Compressed) <> 8 then begin
//        try
//          m_FileStream.Read(EncryptBuffer^, AImgInfo.nSize - 6);
//          ABuffer := Pointer(EncryptBuffer);
//        finally
//          FreeMem(EncryptBuffer);
//        end;
//      end else begin
//        if m_FileStream.Read(EncryptBuffer^, AImgInfo.nSize - 6) =  AImgInfo.nSize - 6 then begin
//          ABuffer := nil;
//          try
//            ZDecompress2(Pointer(EncryptBuffer), AImgInfo.nSize - 6, ABuffer, ASize, -15, 0);
//          finally
//            FreeMem(EncryptBuffer)
//          end;
//        end;
//      end;
//      if ASize + m_FileStream.Position <= m_FileStream.Size then
//      begin
//        try
//          pdximg.surface.LoadFromDataEx(ABuffer, ASize, BytesPerPixe * 8, AImgInfo.m_nWidth, AImgInfo.m_nHeight, False, True);
//        finally
////          FreeMem(ABuffer);
//        end;
//      end;
//
//    end else begin
//      if ASize + m_FileStream.Position <= m_FileStream.Size then
//      begin
//        GetMem(ABuffer, ASize);
//        try
//          m_FileStream.Read(ABuffer^, ASize);
//          pdximg.surface.LoadFromDataEx(ABuffer, ASize, BytesPerPixe * 8, AImgInfo.m_nWidth, AImgInfo.m_nHeight, False, True);
//        finally
//          FreeMem(ABuffer);
//        end;
//      end;
//    end;
//  end;
//end;

procedure TWMImages.ClearCache;
var
  I: integer;
begin
  if FHasData then
  begin
    for i := 0 to ImageCount - 1 do
    begin
      if m_ImgArr[i] <> nil then
      begin
        if m_ImgArr[i].Surface <> nil then
        begin
          try
            m_ImgArr[i].Surface.Free;
            ResourceFreeMem(m_ImgArr[i],SizeOf(TDXImage));
          finally
            m_ImgArr[i] := nil;
          end;
        end;
      end;
    end;
  end;
  FHasData := False;
end;

procedure TWMImages.Finalize;
var
  I: Integer;
begin
  m_IndexList.Clear;

  for I := 0 to ImageCount - 1 do
  begin
    if m_ImgArr[I] <> nil then
    begin
      if m_ImgArr[I].Surface <> nil then
      begin
        try
          m_ImgArr[I].Surface.Free;
          ResourceFreeMem(m_ImgArr[I],SizeOf(TDXImage));
        finally
          m_ImgArr[I] := nil;
        end;
      end;
    end;
  end;

  SetLength(m_ImgArr,0);
  DecMemSize(SizeOf(pTDXImage)* FImageCount);

  if m_FileStream <> nil then
    FreeAndNil(m_FileStream);
end;

function TWMImages.GetCachedImage(index: integer; var m_wPx, m_wPy: integer): TCustomLockableTexture;
var
  position: integer;
  NowTickCount:Cardinal;
begin
  Result := nil;
  //动态初始化
  if not FInitialize then begin
    FInitialize := True;
    Initialize;
    Exit;
  end;

  if Self = nil then Exit;

  if (index < 0) or (index >= ImageCount) then exit;
  NowTickCount := GetTickCount;

  try
    if NowTickCount - m_dwMemChecktTick > 10000 then
    begin
      m_dwMemChecktTick := NowTickCount;
      FreeOldMemorys;
    end;


    if m_ImgArr[index] = nil then
      m_ImgArr[index] := ResourceGetMem(SizeOf(TDXImage));

    if m_ImgArr[index].Surface = nil then
    begin
      //索引 > 0有数据 可以下载 = 0 空图片 = -1支持微端下载
      if index < m_IndexList.Count then
      begin
        position := m_IndexList[index];
        if position > 0 then
        begin
          LoadDxImage(index, position, m_ImgArr[index]);
          FHasData := True;
          m_ImgArr[index].dwLatestTime := NowTickCount;
          m_wPx := m_ImgArr[index].nPx;
          m_wPy := m_ImgArr[index].nPy;
          Result := m_ImgArr[index].Surface;
        end else if position = -1 then
        begin
          if LibManager.FMiniOpend then
          begin
            //10秒没更新成功，重新请求微端
            if NowTickCount - GetLastRequestTime(Index) > 10000 then
            begin
              SetRequestTime(Index,NowTickCount);
              LibManager.AddDownloadImage(FFileName,Index,m_boImportantDown);
            end;
          end;
        end;
      end;
    end
    else
    begin
      m_ImgArr[index].dwLatestTime := GetTickCount;
      m_wPx := m_ImgArr[index].nPx;
      m_wPy := m_ImgArr[index].nPy;
      Result := m_ImgArr[index].Surface;
    end;
    //FReadTime := GetTickCount;
  except
  end;
end;

function TWMImages.GetEmpty: Boolean;
begin
  Result := FImageCount <= 0;
end;

{ TWMPackageImages }

function TWMPackageImages.CheckImageInfoSize(APosition: Integer): Boolean;
begin
  Result := True;
  if APosition <= 0 then
  begin
    Result := False;
    Exit;
  end;

  case FDataHeader.XVersion of
    0:
    begin
      Result := APosition + SizeOf(TPackDataImageInfo) <= m_FileStream.Size;
    end;
    1:
    begin
      Result := APosition + 22 <= m_FileStream.Size;
    end;
  end;
end;

class procedure TWMPackageImages.CreateNewFile(const FileName: String; BitCount: Integer);
var
  AFile: TFileStream;
  ADataHeader: TPackDataHeader;
begin
  AFile :=  TFileStream.Create(FileName, fmCreate);
  try
    ADataHeader.Title :=  '奇奇网络资源文件';
    ADataHeader.ImageCount  :=  0;
    ADataHeader.IndexOffSet :=  SizeOf(TPackDataHeader);
    AFile.Write(ADataHeader, SizeOf(TPackDataHeader));
  finally
    AFile.Free;
  end;
end;

procedure TWMPackageImages.DoGetImgSize(position: integer; var AWidth,
  AHeight: Integer);
var
  ImageInfo: TPackDataImageInfo;
begin
  AWidth := 0;
  AHeight := 0;
  if CheckImageInfoSize(position) then
  begin
    ImageInfo := ReadImageInfo(Position);
    AWidth  :=  ImageInfo.m_nWidth;
    AHeight :=  ImageInfo.m_nHeight;
  end;
end;

procedure TWMPackageImages.DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer);
var
  ImageInfo: TPackDataImageInfo;
begin
  AWidth := 0;
  AHeight := 0;
  m_wPx := 0;
  m_wPy := 0;
  if CheckImageInfoSize(position) then
  begin
    ImageInfo := ReadImageInfo(Position);
    AWidth  :=  ImageInfo.m_nWidth;
    AHeight :=  ImageInfo.m_nHeight;
    m_wPx  :=  ImageInfo.m_wPx;
    m_wPy  :=  ImageInfo.m_wPy;
  end;
end;

function TWMPackageImages.GetImgDataSize(Index: Integer): Integer;
var
  Position : Integer;
  ImageInfo: TPackDataImageInfo;
  OldPosition:Int64;
  nIndex : Integer;
begin
  Result := 0;
  OldPosition := m_FileStream.Position;
  if (Index >= 0) or (Index < m_IndexList.Count ) then
  begin
    Position := m_IndexList[Index];
    if Position = 0 then  Exit;

    if not IsEncrypt then
    begin
      if CheckImageInfoSize(Position) then
      begin
        ImageInfo := ReadImageInfo(Position);
        case  FDataHeader.XVersion of
          0: Result := ImageInfo.m_Len + SizeOf(ImageInfo);
          1: Result := ImageInfo.m_Len + 22;
        end;
      end;
    end else
    begin
      if not m_boSortedIndex then
        SortIndex();

      nIndex := m_SortedIndexPosition.IndexOf(Position);
      while true do
      begin
        nIndex := nIndex + 1;
        if nIndex >= m_SortedIndexPosition.Count then
        begin
          Result := m_FileStream.Size - Position;
          Break;
        end;

        if m_SortedIndexPosition[nIndex] > Position then
        begin
          Result := m_SortedIndexPosition[nIndex] - Position;
          Break;
        end;
      end;
    end;
    m_FileStream.Position := OldPosition; //还原原来的游标
  end;
end;

function TWMPackageImages.GetIsEncrypt: Boolean;
begin
  {$IF DEFINED(MINIRESBUILDER) or DEFINED(DEVMODE)}
  Result := False;
  {$ELSE}
  Result := FDataHeader.XVersion = 1;
  {$IFEND}
end;

procedure TWMPackageImages.Initialize;
begin
  inherited;
  if FImageCount = - 1 then
  begin
    if m_FileStream = nil then
    begin
      m_FileStream := CreateFileStream(FFileName, fmOpenRead or fmShareDenyNone);
      if m_FileStream = nil then
      begin
        if MiniCreate then
        begin
          Try
//            m_FileStream := TFileStream.Create(FFileName,fmCreate);
//            m_FileStream.Free;
          except

          End;
          LibManager.AddDownloadImage(FFileName,0,m_boImportantDown);
        end;
        Exit;
      end;
    end;
    FDataHeader := Self.ReadHeader;
    LibManager.FPassWordList.TryGetValue(UpperCase(FFileName),m_PassWord);
    if not IsEncrypt or (IsEncrypt and (FDataHeader.Password = m_PassWord)) then
    begin
      FImageCount :=  FDataHeader.ImageCount;
      if FDataHeader.IndexOffSet = 0 then
        FDataHeader.IndexOffSet := SizeOf(TPackDataHeader);
      SetLength(m_ImgArr,FImageCount);
      AddMemSize(SizeOf(pTDxImage) * FImageCount);
      LoadIndex;
      FInitialize := True;
    end else
    begin
      ConsoleDebug(Format('文件:%s ,密码:%s 不对,正确的密码是:%s',[FFileName,m_PassWord,FDataHeader.Password]));
    end;
  end;
end;

procedure TWMPackageImages.LoadDxImage(Index, position: integer; pdximg: PTDxImage);
var
  ImageInfo: TPackDataImageInfo;
  AZlibBuf, ABuf: PAnsiChar;
  ASize: Integer;
  Png : TPngImage;
  pPngData:PByte;
begin
  if CheckImageInfoSize(position) then
  begin
    ImageInfo := ReadImageInfo(Position);
    if (ImageInfo.m_nWidth * ImageInfo.m_nHeight <= 0) then Exit;
    pdximg.nPx := ImageInfo.m_wPx;
    pdximg.nPy := ImageInfo.m_wPy;

    pdximg.surface := PXL.Providers.TGraphicsDeviceProvider(Device.Provider).CreateLockableTexture(Device, False);
    if ImageInfo.m_Len + m_FileStream.Position <= m_FileStream.Size then
    begin
      case ImageInfo.GraphicType of
        gtRealPng:
        begin
          Png := TPngImage.Create;
          Try
            Png.LoadFromStream(m_FileStream);
            pdximg.surface.LoadFromPng(Png);
          Finally
            Png.Free;
          End;
        end;
        gtPng:
        begin
          GetMem(AZlibBuf, ImageInfo.m_Len);
          Try
            m_FileStream.Read(AZlibBuf^, ImageInfo.m_Len);
            DecompressBufZ(AZlibBuf, ImageInfo.m_Len, 0, ABuf, ASize);
            if ImageInfo.bitCount = 32 then
              pdximg.surface.LoadFromPng32Data(ABuf, ASize, ImageInfo.bitCount, ImageInfo.m_nWidth, ImageInfo.m_nHeight)
            else
              pdximg.surface.LoadFromDataEx(ABuf, ASize, ImageInfo.bitCount, ImageInfo.m_nWidth, ImageInfo.m_nHeight, False, False);
          Finally
            FreeMem(ABuf, ASize);
            FreeMem(AZlibBuf);
          End;
        end else
        begin
          GetMem(AZlibBuf, ImageInfo.m_Len);
          Try
            m_FileStream.Read(AZlibBuf^, ImageInfo.m_Len);
            DecompressBufZ(AZlibBuf, ImageInfo.m_Len, 0, ABuf, ASize);
            pdximg.surface.LoadFromDataEx(ABuf, ASize, ImageInfo.bitCount, ImageInfo.m_nWidth, ImageInfo.m_nHeight, False, False);
          Finally
            FreeMem(ABuf, ASize);
            FreeMem(AZlibBuf);
          End;
        end;
      end;
    end;
  end;
end;

procedure TWMPackageImages.LoadIndex;
var
  I, Value: Integer;
  Buf: PAnsiChar;
  BufSize: Integer;
begin
  m_IndexList.Clear;
  m_FileStream.Position := FDataHeader.IndexOffSet;
  BufSize := m_FileStream.Size - m_FileStream.Position;
  GetMem(Buf, BufSize);
  try
    m_FileStream.ReadBuffer(Buf^, BufSize);
    for I := 0 to FImageCount - 1 do
    begin
      if I * 4 < BufSize then
        Move(Buf[I * 4], Value, 4)
      else
        Value := 0;
      m_IndexList.Add(Value);
    end;
  finally
    FreeMem(Buf);
  end;
end;

function TWMPackageImages.ReadHeader: TPackDataHeader;
begin
  Result := ReadFileHeader(m_FileStream);
end;

class function TWMPackageImages.ReadFileHeader(AStream: TStream): TPackDataHeader;
var
  AEncData,
  ADecData: PAnsiChar;
  ADecSize: Integer;
begin
  FillChar(Result, SizeOf(TPackDataHeader), #0);
  AStream.Position := 0;
  GetMem(AEncData, 80);
  try
    AStream.ReadBuffer(AEncData^, 80);
    uEDCode.DecodeData(AEncData^, 80, ADecData, ADecSize, _HKey);
    Move(ADecData^, Result, SizeOf(TPackDataHeader));
  finally
    FreeMem(AEncData, 80);
    FreeMem(ADecData, ADecSize);
  end;
end;

function TWMPackageImages.ReadImageInfo(APosition: Integer): TPackDataImageInfo;
var
  AEncData,
  ADecData: PAnsiChar;
  ADecSize: Integer;
begin
  m_FileStream.Position := APosition;
  case FDataHeader.XVersion of
    0:
    begin
      m_FileStream.ReadBuffer(Result, SizeOf(TPackDataImageInfo));
    end;
    1:
    begin
      GetMem(AEncData, 22);
      try
        m_FileStream.ReadBuffer(AEncData^, 22);
        uEDCode.DecodeData(AEncData^, 22, ADecData, ADecSize, _Key);
        Move(ADecData^, Result, SizeOf(TPackDataImageInfo));
      finally
        FreeMem(AEncData, 22);
        FreeMem(ADecData, ADecSize);
      end;
    end;
  end;
end;

procedure TWMPackageImages.Reload;
begin
  inherited;
  if FImageCount = -1 then
  begin
    Initialize;
  end else
  begin
    if m_FileStream <> nil then
      m_FileStream.Free;

    m_FileStream := CreateFileStream(Self.FileName,fmOpenRead or fmShareDenyNone);

    FDataHeader := Self.ReadHeader;
    if not IsEncrypt or (IsEncrypt and (FDataHeader.Password = m_PassWord)) then
    begin
      FImageCount :=  FDataHeader.ImageCount;
      if FDataHeader.IndexOffSet = 0 then
        FDataHeader.IndexOffSet := SizeOf(TPackDataHeader);

      LoadIndex();
    end;
  end;
end;

function SortDataIndex(const Left, Right: Integer): Integer;
begin
  if Left = Right then
  begin
    Result := 0;
  end else if Left > Right then
  begin
    Result := 1;
  end else if Left < Right then
  begin
    Result := -1;
  end;
end;

procedure TWMPackageImages.SortIndex;
 var
  I :Integer;
  Compare : IComparer<Integer>;
begin
  if m_SortedIndexPosition <> nil then
    m_SortedIndexPosition.Free;
  m_SortedIndexPosition := TList<Integer>.Create;

  for i := 0 to ImageCount - 1 do
  begin
    m_SortedIndexPosition.Add(Self.Index[i]);
  end;
  Compare := TComparer<Integer>.Construct(SortDataIndex);
  m_SortedIndexPosition.Sort(Compare);
  m_boSortedIndex := True;
end;

class procedure TWMPackageImages.WriteHeader(AStream: TStream;
  AHeader: TPackDataHeader);
var
  AEncData: PAnsiChar;
  AEncSize: integer;
begin
  try
    uEDCode.EncodeData(AHeader, SizeOf(TPackDataHeader), _HKey, AEncData,
      AEncSize);
    AStream.position := 0;
    AStream.WriteBuffer(AEncData^, AEncSize);
  finally
    FreeMem(AEncData, AEncSize);
  end;
end;

{ TWZLImages }

procedure TWZLImages.DoGetImgSize(position: integer; var AWidth,
  AHeight: Integer);
var
  ImageInfo: TWZLImageInfo;
begin
  AWidth := 0;
  AHeight := 0;
  if position + SizeOf(TWZLImageInfo) <= m_FileStream.Size then
  begin
    m_FileStream.Seek(position, 0);
    m_FileStream.Read(ImageInfo, Sizeof(TWZLImageInfo));
    AWidth  :=  ImageInfo.m_nWidth;
    AHeight :=  ImageInfo.m_nHeight;
  end;
end;

procedure TWZLImages.DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer);
var
  ImageInfo: TWZLImageInfo;
begin
  AWidth := 0;
  AHeight := 0;
  m_wPx := 0;
  m_wPy := 0;
  if position + SizeOf(TWZLImageInfo) <= m_FileStream.Size then
  begin
    m_FileStream.Seek(position, 0);
    m_FileStream.Read(ImageInfo, Sizeof(TWZLImageInfo));
    AWidth  :=  ImageInfo.m_nWidth;
    AHeight :=  ImageInfo.m_nHeight;
    m_wPx  :=  ImageInfo.m_wPx;
    m_wPy  :=  ImageInfo.m_wPy;
  end;
end;

procedure TWZLImages.Extract(Index: Integer; var AWidth, AHeight,
  DataLen: Integer; var Data: PAnsiChar);
var
  ImageInfo: TWZLImageInfo;
  AData: PAnsiChar;
begin
  m_FileStream.Position := m_IndexList[index];
  m_FileStream.Read(ImageInfo, SizeOf(TWZLImageInfo));
  AWidth := ImageInfo.m_nWidth;
  AHeight := ImageInfo.m_nHeight;
  DataLen := 0;
  if ImageInfo.m_Len > 0 then
  begin
    if ImageInfo.m_Len + m_FileStream.Position <= m_FileStream.Size then
    begin
      GetMem(AData, ImageInfo.m_Len);
      try
        m_FileStream.Read(AData^, ImageInfo.m_Len);
        DecompressBufZ(AData, ImageInfo.m_Len, 0, Data, DataLen);
      finally
        FreeMem(AData);
      end;
    end;
  end;
end;

function TWZLImages.GetImgDataSize(Index: Integer): Integer;
var
  ImageInfo: TWZLImageInfo;
  BitSize:Byte;
  calcLen:Integer;
  OldPosition:Integer;
begin
  Result := 0;
  if m_IndexList[Index] > 0 then
  begin
    OldPosition :=  m_FileStream.Position;
    m_FileStream.Position := m_IndexList[index];
    Try
      if m_FileStream.Read(ImageInfo, SizeOf(TWZLImageInfo)) = SizeOf(TWZLImageInfo) then
      begin
        if ImageInfo.m_Len > 0 then
        begin
          Result := ImageInfo.m_Len + SizeOf(TWZLImageInfo);
        end else
        begin
          case ImageInfo.m_Enc1 of
            3: BitSize := 8;
            5: BitSize := 16;
            6:begin
               BitSize := 24;
            end;
           7:begin
               BitSize := 32;
            end;
            else
            begin
              BitSize := 0;
            end;
              Exit;
          end;
          if BitSize > 0 then
          begin
            calcLen := ImageInfo.m_nWidth * ImageInfo.m_nHeight * (BitSize div 8);
            if calcLen + m_FileStream.Position <= m_FileStream.Size then
            begin
              Result := calcLen + SizeOf(TWZLImageInfo);
            end;
          end;
        end;
      end;
    Finally
      m_FileStream.Position := OldPosition;
    End;
  end;
end;

procedure TWZLImages.Initialize;
var
  Idxfile: string;
  FImageHeader: TWZLImageHeader;
begin
  inherited;

  if FImageCount = - 1 then
  begin
    if m_FileStream = nil then
    begin
      m_FileStream := CreateFileStream(FFileName,fmOpenRead or fmShareDenyNone);
      if m_FileStream = nil then
      begin
        if MiniCreate then
        begin
          Try
//            m_FileStream := TFileStream.Create(FFileName,fmCreate);
//            m_FileStream.Free;
          except

          End;
          //微端建立的请求下载文件。
          LibManager.AddDownloadImage(FFileName,0,m_boImportantDown);
        end;
        Exit; //非微端建立 文件不存在退出执行。
      end;
    end;
    m_FileStream.Position := 0;

    m_FileStream.Read(FImageHeader, SizeOf(TWZLImageHeader));
    FImageCount :=  FImageHeader.IndexCount;

    //m_ImgArr := ResourceGetMem(SizeOf(TDxImage) * FImageCount);
    SetLength(m_ImgArr,FImageCount);
    AddMemSize(SizeOf(pTDxImage) * FImageCount);

    idxfile :=  ExtractFilePath(FFileName) + ExtractFileNameOnly(FFileName) + '.WZX';
    LoadIndex(idxfile);
    FInitialize := True;
  end;
end;

procedure TWZLImages.LoadDxImage(Index, position: integer; pdximg: PTDxImage);
var
  ImageInfo: TWZLImageInfo;
  DBits, PInBits: PAnsiChar;
  OutSize: Integer;
  BitSize: Integer;
begin
  try
    if position + SizeOf(TWZLImageInfo) <= m_FileStream.Size then
    begin
      m_FileStream.Position := Position;
      m_FileStream.Read(ImageInfo, SizeOf(TWZLImageInfo));
      BitSize := 16;
      case ImageInfo.m_Enc1 of
        3: BitSize := 8;
        5: BitSize := 16;
        6:begin
          BitSize := 24;
        end;
       7:begin
          BitSize := 32;
        end;
        else
          Exit;
      end;
      pdximg.nPx := ImageInfo.m_wPx;
      pdximg.nPy := ImageInfo.m_wPy;
      pdximg.surface := PXL.Providers.TGraphicsDeviceProvider(Device.Provider).CreateLockableTexture(Device, False);
      if ImageInfo.m_Len > 0 then
      begin
        if ImageInfo.m_Len + m_FileStream.Position <= m_FileStream.Size then
        begin
          GetMem(PInBits, ImageInfo.m_Len);
          try
            m_FileStream.Read(PInBits^, ImageInfo.m_Len);
            DecompressBufZ(PInBits, ImageInfo.m_Len, 0, DBits, OutSize);
            if ImageInfo.m_Enc2 = 9 then
              pdximg.surface.LoadAlphaFromDataEx(DBits, OutSize, ImageInfo.m_nWidth, ImageInfo.m_nHeight)
            else
              pdximg.surface.LoadFromDataEx(DBits, OutSize, BitSize, ImageInfo.m_nWidth, ImageInfo.m_nHeight, False, True);
          finally
            FreeMem(PInBits);
            FreeMem(DBits);
          end;
        end;
      end
      else
      begin
        if ImageInfo.m_nWidth * ImageInfo.m_nHeight * (BitSize div 8) + m_FileStream.Position <= m_FileStream.Size then
        begin
          OutSize := ImageInfo.m_nWidth * ImageInfo.m_nHeight * (BitSize div 8);
          GetMem(DBits, OutSize);
          try
            m_FileStream.Read(DBits^, OutSize);
            pdximg.surface.LoadFromDataEx(DBits, OutSize, BitSize, ImageInfo.m_nWidth, ImageInfo.m_nHeight, False, True);
          finally
            FreeMem(DBits);
          end;
        end;
      end;
    end;
  except
  end;
end;

procedure TWZLImages.LoadIndex(const FileName: string);
var
  I, Value: integer;
  IdxHeader: TWZXIndexHeader;
  F: TFileStream;
  Buf: PAnsiChar;
  BufSize: Integer;
begin
  m_IndexList.Clear;
  if FileExists(FileName) then
  begin
    F := CreateFileStream(FileName, fmOpenRead or fmShareDenyNone);
    if F = nil then
      Exit;

    try
      F.ReadBuffer(IdxHeader, SizeOf(TWZXIndexHeader));
      BufSize := F.Size - F.Position;
      GetMem(Buf, BufSize);
      try
        F.ReadBuffer(Buf^, BufSize);
        for I := 0 to FImageCount - 1 do
        begin
          if I * 4 < BufSize then
            Move(Buf[I * 4], Value, 4)
          else
            Value := 0;
          m_IndexList.Add(Value);
        end;
      finally
        FreeMem(Buf);
      end;
    finally
      F.Free;
    end;
  end;
end;

procedure TWZLImages.Reload;
var
  WzxFileName:String;
begin
  inherited;
  if FImageCount = -1 then
  begin
    Initialize;
  end else
  begin
    WzxFileName := ChangeFileExt(Self.FileName,'.wzx');
    LoadIndex(WzxFileName);
    if m_FileStream <> nil then
      m_FileStream.Free;
    m_FileStream := CreateFileStream(Self.FileName,fmOpenRead or fmShareDenyNone );
  end;
end;

{ TWMImages }

constructor TWMImages.Create(ADevice:TCustomDevice);
begin
  Device:= ADevice;
  FFileName   := '';
  FImageCount := -1;
  m_dwMemChecktTick := GetTickCount;

  m_FileStream := nil;
  m_ImgArr := nil;
  FVT := False;
  m_IndexList := TList<Integer>.Create;
  FReadTime := GetTickCount;
  FHasData := False;
  FInitialize := False;

  AllWMImages.Add(Self);
end;

class procedure TWMImages.CreateNewFile(const FileName: String; BitCount: Integer);
begin
end;

class procedure TWMImages.DecMemSize(Size: Cardinal);
begin
  MemmoryUsage := MemmoryUsage - Size;
end;

class procedure TWMImages.AddMemSize(Size: Cardinal);
begin
  MemmoryUsage := MemmoryUsage + Size;
end;

destructor TWMImages.Destroy;
begin
  Finalize;
  if m_FileStream <> nil then
    FreeAndNil(m_FileStream);
  m_IndexList.Free;
end;

procedure TWMImages.DoGetImgSize(position: integer; var AWidth,
  AHeight: Integer);
begin

end;

procedure TWMImages.DoGetImgSize(position: integer; var AWidth, AHeight, m_wPx, m_wPy: Integer);
begin
end;

procedure TWMImages.Extract(Index: Integer; var AWidth, AHeight,
  DataLen: Integer; var Data: PAnsiChar);
begin
  DataLen := 0;
end;

procedure TWMImages.FreeOldMemorys;
var
  I: integer;
  ACurTick: LongWord;
begin
  ACurTick := GetTickCount;
  for I := 0 to ImageCount - 1 do
  begin
    if m_ImgArr[I] <> nil then
    begin
      if m_ImgArr[I].Surface <> nil then
      begin
        if ACurTick - m_ImgArr[I].dwLatestTime > FREE_TEXTURE_TIME then
        begin
          try
            m_ImgArr[I].Surface.Free;
            ResourceFreeMem(m_ImgArr[I],SizeOf(TDXImage));
          finally
             m_ImgArr[I] := nil;
          end;
        end;
      end;
    end;
  end;
end;

function TWMImages.GetImageSurface(index: integer): TCustomLockableTexture;
var
  nX,nY:Integer;
begin
  Result := GetCachedImage(index,nX,nY);
end;

function TWMImages.GetImgSize(index: Integer; var AWidth,
  AHeight: Integer): Boolean;
var
  nPosition: Integer;
begin
  Result  :=  False;
  if (index < 0) or (index >= m_IndexList.Count ) then Exit;

  nPosition := m_IndexList[index];
  if nPosition > 0 then
  begin
    DoGetImgSize(nPosition, AWidth, AHeight);
    Result  :=  True;
  end
  else
  begin
    AWidth := 0;
    AHeight := 0;
  end;
end;

function TWMImages.GetImgDataSize(Index: Integer): Integer;
begin
  Result := 0;
end;

function TWMImages.GetImgSize(index: Integer; var AWidth, AHeight, m_wPx, m_wPy: Integer): Boolean;
var
  nPosition: Integer;
begin
  Result  :=  False;
  if (index < 0) or (index >= ImageCount) then exit;
  nPosition := m_IndexList[index];
  if nPosition > 0 then
  begin
    DoGetImgSize(nPosition, AWidth, AHeight, m_wPx, m_wPy);
    Result  :=  True;
  end
  else
  begin
    AWidth := 0;
    AHeight := 0;
    m_wPx := 0;
    m_wPy := 0;
  end;
end;

function TWMImages.GetIndex(index: Integer): Integer;
begin
  Result := 0;
  if (index < 0) or (index >= ImageCount) then exit;
  Result := m_IndexList[index];;
end;

function TWMImages.GetLastRequestTime(Index: Integer): Cardinal;
var
  Part:Integer;
begin
  Part := GetPartByIndex(Index);
  if Part >= 0 then
  begin
    Result := m_PartInfo.FPartRequestTick[Part];
  end else
  begin
    Result := GetTickCount;
  end;
end;

function TWMImages.GetPartByIndex(Index: Integer): Integer;
begin
  Result := - 1;

  if (Index >= 0) and (m_PartInfo <> nil ) and (Index < m_PartInfo.GetPartCount) then
  begin
    Result := m_PartInfo.FPartIndex[Index];
  end;
end;

procedure TWMImages.Initialize;
begin
  if FFileName = '' then
    raise Exception.Create('FileName not assigned..');

    //WZL DATA 是支持微端下载的。 这里不需要对其进行判定。
  if (not FileExists(FFileName)) and not (Self is TWZLImages) and not (Self is TWMPackageImages) then
    raise Exception.Create('File not found:'+ FFileName);

   if not LibManager.FPartInfo.TryGetValue(UpperCase(FFileName),m_PartInfo) then
   begin
     if m_boMiniCreate then
     begin
       raise Exception.Create('通过微端建立还没有找到微端信息描述 :' + FFileName);
     end;
   end;
end;

procedure TWMImages.LoadDxImage(Index, position: integer; pdximg: PTDxImage);
begin
end;

procedure TWMImages.ReLoad;
begin

end;

class procedure TWMImages.ResourceFreeMem(P: Pointer; Size: Cardinal);
begin
  if P <> Nil then
  begin
    DecMemSize(Size);
    FreeMem(P,Size);
  end;
end;

class function TWMImages.ResourceGetMem(Size: Cardinal): Pointer;
begin
  Result := AllocMem(Size);
  if Result <> nil then
  begin
    AddMemSize(Size);
  end;
end;

procedure TWMImages.SetFileName(const Value: String);
var
  AExt,
  AName: String;
begin

  FFileName := StringReplace(Value, '\\', '\', [rfReplaceAll]);
  AName := ExtractFileName(FFileName);
  AExt := ExtractFileExt(FFileName);
  if Uppercase(AExt) = '.DATA' then
    LibManager.AddLib(UpperCase(FFileName), Self)
  else
  begin
    LibManager.AddLib(UpperCase(ChangeFileExt(FFileName, '')), Self);
  end;
end;

procedure TWMImages.SetRequestTime(index: Integer; Time: Cardinal);
var
  Part : Integer;
begin
  Part := GetPartByIndex(Index);
  if Part >= 0 then
  begin
    m_PartInfo.FPartRequestTick[Part] := Time;
  end;
end;

{ TWMImageLibManager }

procedure TWMImageLibManager.AddLib(const AName: String; Lib: TWMImages);
begin
  FLibs.AddOrSetValue(AName, Lib);
  FFullNameLibs.AddOrSetValue(UpperCase(Lib.FFileName), Lib);
  FMiniUseLibs.AddOrSetValue(ChangeFileExt(AName,''),Lib);
end;

procedure TWMImageLibManager.AddPassWord(const FileName, PassWord: String);
begin
  FPassWordList.AddOrSetValue(UpperCase(FileName),PassWord);
end;

procedure TWMImageLibManager.ClearTextures;
var
  AItems: TDictionary<String, TWMImages>.TPairEnumerator;
begin
  AItems  :=  FLibs.GetEnumerator;
  try
    while AItems.MoveNext do
    begin
      try
        AItems.Current.Value.ClearCache;
      except
      end;
    end;
  finally
    AItems.Free;
  end;
end;

procedure TWMImageLibManager.AddDownloadImage(const FileName: String; ImageIndex: Integer ; Important:Boolean);
begin
  if Assigned(FAddDownloadImage) then
  begin
    FAddDownloadImage(FileName, ImageIndex,Important);
    ConsoleDebug(Format('请求下载文件:%s ,图片序号:%d 。 OK!',[FileName,ImageIndex]));
  end else
  begin
    ConsoleDebug(Format('在没有绑定请求回调下 请求下载文件:%s ,图片序号:%d  无效',[FileName,ImageIndex]));
  end;
end;

constructor TWMImageLibManager.Create;
begin
  FLibs :=  TDictionary<String, TWMImages>.Create;
  FFullNameLibs :=  TDictionary<String, TWMImages>.Create;
  FMiniUseLibs := TDictionary<String, TWMImages>.Create;
  TWMImages.AllWMImages := TList<TWMImages>.Create;
  FMiniImageFile := TDictionary<String, String>.Create;
  FPartInfo := TDictionary<String,PTImageFilePartInfoLite>.Create;
  FPassWordList := TDictionary<String, String>.Create;
end;

destructor TWMImageLibManager.Destroy;
begin
  FLibs.Free;
  FFullNameLibs.Free;
  TWMImages.AllWMImages.Free;
  FMiniUseLibs.Free;
  FMiniImageFile.Free;
  FPartInfo.Free;
  FPassWordList.Free;
  inherited;
end;

procedure TWMImageLibManager.LoadImage(const FileName: String; Index,
  Position: Integer);
var
  Lib: TWMImages;
begin
  if TryGetLibByFullName(UpperCase(FileName), Lib) then
  begin
    Lib.m_IndexList[Index] := Position;
  end;
end;

procedure TWMImageLibManager.LoadLibNames(ANames: TStrings);
var
  AItems: TArray<TPair<String, TWMImages>>;
  I: Integer;
  AFileName: String;
begin
  AItems := FLibs.ToArray;
  for I := Low(AItems) to High(AItems) do
  begin
    if not AItems[I].Value.Empty then
    begin
      AFileName := AItems[I].Value.FFileName;
      if Pos(Uppercase(MIRPATH), Uppercase(AFileName)) = 1 then
        Delete(AFileName, 1, Length(MIRPATH));
      if Pos(Uppercase(ResDir), Uppercase(AFileName)) = 1 then
      begin
        Delete(AFileName, 1, Length(ResDir));
        AFileName := '$' + AFileName;
      end;
      ANames.Add(AFileName);
    end;
  end;
end;

function TWMImageLibManager.LoadMiniImageFile(const FileName: String):Boolean;
var
  I :Integer;
  ResVer:TMiniResFilePackage;
  HashFileName:String;
begin
  Result := False;
  ResVer := TMiniResFilePackage.Create;
  Try
    ResVer.CheckPassword := False;
    ResVer.LoadPackageFromFile(FileName,'');
    if ResVer.Inited then
    begin
      //获取文件列表
      ResVer.EachImage(procedure (const FileName:String;Stream : TImageFilePartInfo)
      begin
        HashFileName := UpperCase(ChangeFileExt(FileName,''));
        if Stream.Format = wWZL then
          FMiniImageFile.AddOrSetValue(HashFileName,ChangeFileExt(FileName,'.wzl'))
        else
          FMiniImageFile.AddOrSetValue(HashFileName,ChangeFileExt(FileName,'.data'));
      end );

      //获取对应的ImageFilePartInfo_Lite
      ResVer.EachImage(procedure (const FileName:String;Stream : TImageFilePartInfo)
      begin
         Self.LoadPartInfoFile(Stream)
      end );

      Result := True;
    end;
  Finally
    ResVer.Free;
  End;
end;

function TWMImageLibManager.LoadPartInfoFile(
  const Value: TImageFilePartInfo): Integer;
var
  FileName:String;
  PartInfoLite:PTImageFilePartInfoLite;
  I:Integer;
begin
  if Value.Format = wWZL then
    FileName := UpperCase(Value.FileName) + '.WZL'
  else
    FileName := UpperCase(Value.FileName) + '.DATA';

  if not FPartInfo.TryGetValue(FileName,PartInfoLite) then
  begin
    New(PartInfoLite);
  end;
  SetLength(PartInfoLite.FPartIndex,Value.ImageCount);
  SetLength(PartInfoLite.FPartRequestTick,Value.PartCount);

  for i := 0 to Value.ImageCount - 1 do
  begin
    PartInfoLite.FPartIndex[i] := Value.GetIndexPart(i);
  end;

  if Pos('91',FileName) > 0 then
    FPartInfo.Add(FileName,PartInfoLite)
  else
    FPartInfo.Add(FileName,PartInfoLite);
end;

procedure TWMImageLibManager.ReLoadImageFile(const FileName: String);
var
  HashFileName:String;
  Image:TWMImages;
begin
  HashFileName := ChangeFileExt(UpperCase(FileName),'');
  Try
    if FMiniUseLibs.TryGetValue(HashFileName,Image) then
    begin
      Image.ReLoad;
    end;
  Finally
    FileReLoadFinish(FileName);
  End;
end;

procedure TWMImageLibManager.SetMiniOpen(Value: Boolean);
begin
  FMiniOpend := Value;
  if Value then
    ConsoleDebug('设置微端功能开启')
  else
    ConsoleDebug('设置微端功能关闭');
end;

function TWMImageLibManager.TryGetLib(const AName: String; out Lib: TWMImages): Boolean;
var
  S: String;
begin
  Result := False;
  Lib := nil;

  if AName = '' then exit;

  S := UpperCase(NameExpandToFileName(AName));
  if ExtractFileExt(S) <> '.DATA' then
    S := ChangeFileExt(S, '');
  Result := FLibs.TryGetValue(S, Lib);
  if not Result then
  begin
    Lib := CreateImages(AName, g_GameDevice);
    if Lib <> nil then
    begin
      Result := True;
      Lib.Initialize;
    end;
  end;
end;

function TWMImageLibManager.TryGetLib2(const AName: String; out Lib: TWMImages): Boolean;
var
  S: String;
begin
  Result := False;
  Lib := nil;
  S := UpperCase(NameExpandToFileName(AName));
  if ExtractFileExt(S) <> '.DATA' then
    S := ChangeFileExt(S, '');
  Result := FLibs.TryGetValue(S, Lib);
  if not Result then
  begin
    Lib := CreateImages(S, g_GameDevice);
    if Lib <> nil then
    begin
      Result := True;
      Lib.Initialize;
    end;
  end;
end;

function TWMImageLibManager.TryGetLibSatic(const AName: String;
  out Lib: TWMImages): Boolean;
var
  S: String;
begin
  Result := False;
  Lib := nil;
  S := UpperCase(AName);
  if ExtractFileExt(S) <> '.DATA' then
    S := ChangeFileExt(S, '');
  Result := FLibs.TryGetValue(S, Lib);
end;

function TWMImageLibManager.TryGetLibByFullName(const AName: String; out Lib: TWMImages): Boolean;
begin
  Result := FFullNameLibs.TryGetValue(AName, Lib);
end;

procedure TWMImageLibManager.FileReLoadFinish(const FileName: String);
begin
  ConsoleDebug('文件重新读取完毕:' + FileName );
  if Assigned(FReloadFinished) then
    FReloadFinished(FileName);
end;

procedure TWMImageLibManager.FreeAllTextureMemory;
var
  AItems: TDictionary<String, TWMImages>.TPairEnumerator;
  NowTick:Cardinal;
  LoopCount:Integer;
begin
  AItems  :=  FLibs.GetEnumerator;
  NowTick := GetTickCount;
  LoopCount := 0;
  try
    while AItems.MoveNext do
    begin
      AItems.Current.Value.FreeOldMemorys;
      AItems.Current.Value.FReadTime := NowTick;
    end;
  finally
    AItems.Free;
  end;
end;

procedure TWMImageLibManager.FreeMemory;
var
  AItems: TDictionary<String, TWMImages>.TPairEnumerator;
  NowTick:Cardinal;
  LoopCount:Integer;
begin
  AItems  :=  FLibs.GetEnumerator;
  NowTick := GetTickCount;
  LoopCount := 0;
  try
    while AItems.MoveNext do
    begin
      try
        if NowTick - AItems.Current.Value.ReadTime > CHECK_FREE_LIB_TIME then
        begin
          AItems.Current.Value.FreeOldMemorys;
          AItems.Current.Value.FReadTime := NowTick;
        end;

        LoopCount := LoopCount + 1;

        //每处理 20 个文件 检测一下是否超时  超时就跳出来。 别导致程序卡了
        if (LoopCount mod 20) = 0 then
        begin
          if GetTickCount - NowTick > 30 then
          begin
            Break;
          end;
        end;
      except
      end;
    end;
  finally
    AItems.Free;
  end;
end;

function TWMImageLibManager.GetMiniFileName(const FileName: String): String;
var
  HashFileName:String;
begin
  HashFileName := UpperCase(ChangeFileExt(FileName,''));
  Result := '';
  FMiniImageFile.TryGetValue(HashFileName,Result);
end;

{ TWMVirtualImages }

constructor TWMVirtualImages.Create;
begin
  inherited;
  FVT := True;
end;

procedure TWMVirtualImages.DoGetImgSize(position: integer; var AWidth, AHeight: Integer);
//var
//  Source: TPictureCollectionItem;
begin
//  if FImageList = nil then Exit;
//  if (position>0) and (position<=FImageList.Items.Count) then
//  begin
//    Source  :=  FImageList.Items.Items[position-1];
//    AWidth  :=  Source.Width;
//    AHeight :=  Source.Height;
//  end;
end;

procedure TWMVirtualImages.Initialize;
var
  I: Integer;
begin
//  if FImageList = nil then Exit;
//  m_IndexList.Clear;
//  FImageCount :=  FImageList.Items.Count;
//  m_ImgArr:=AllocMem(SizeOf(TDxImage) * FImageCount);
//  for I := 0 to FImageList.Items.Count - 1 do
//    m_IndexList.Add(I + 1);
end;

procedure TWMVirtualImages.LoadDxImage(Index, position: integer; pdximg: PTDxImage);
//var
//  Source: TPictureCollectionItem;
begin
//  if FImageList = nil then Exit;
//  if (position>0) and (position<=FImageList.Items.Count) then
//  begin
//    Source  :=  FImageList.Items.Items[position-1];
//    pdximg.nPx := Source.Width;
//    pdximg.nPy := Source.Height;
//    pdximg.State := 0;
//    pdximg.surface := Factory.CreateLockableTexture;
//  end;
end;

{ TImageFilePartInfoLite }

function TImageFilePartInfoLite.GetPartCount: Integer;
begin
  Result := Length(FPartIndex);
end;

initialization
  LibManager  :=  TWMImageLibManager.Create;
  if WMImageInfoSize <> SizeOf(TWMImageInfo) then
    raise Exception.Create('SizeOf "TWMImageInfo" is inaccurate.');
  if WZLImageInfoSize <> SizeOf(TWZLImageInfo) then
    raise Exception.Create('SizeOf "TWZLImageInfo" is inaccurate.');
  if PackDataImageInfoSize <> SizeOf(TPackDataImageInfo) then
    raise Exception.Create('SizeOf "TDataImageInfo" is inaccurate.'+inttostr(SizeOf(TPackDataImageInfo)));

finalization
  if ClientFileHash <> nil then
    ClientFileHash.Free;
  LibManager.Free;

end.

