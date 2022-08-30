unit uMiniResFileInfo;

interface

uses
  Classes, SysUtils, Zlib, Generics.Collections, Dialogs,System.Hash, uEDCode, Windows;

const
  MINIDATA_CHECKCODE :Cardinal = $FFBBCCDD; //微端Data描述文件文件头.
  MINIWZL_CHECKCODE :Cardinal = $FFCCBBEE; //微端 WZL描述文件 文件头

type
  TWZLDATAFormat = (wNone,wWZL,wDATA);
  TSoundFormat = (sfNone,sfWav,sfMp3,sfOgg);

  TMiniResFileInfoItem = packed record
    FileSize: Integer;
    FileNameSize:Integer;
    //ByteArray of Utf8Encode Str.
    //FileByteArray;
  end;

  TPartStartEnd = packed record
    StartIndex,EndIndex:Integer;
  end;

  TPartState = (psEmpty,psDowing,PsMerge,PsOk,psError);
  //一个文件对应的分割部分的记录
  TImageFilePartInfo = class
  private
    FImageIndexPart:Array of Integer;
    FPartStartEnd:Array of TPartStartEnd;
    FPartCount:Integer;
    FPartDownState:Array of TPartState;
    FImageCount:Integer;
    FMD5:string;
    FFormat: TWZLDATAFormat;
    FDataHeader : array[1..80] of Byte;
    FFileName:String;
    FPartRequestTime : Array of Cardinal; //每个部分请求的时间。
    function GetPartState(index:Integer):TPartState;
    procedure SetPartStat(index:Integer;value:TPartState);
    function GetPartRequestTime(PartIndex:Word):Cardinal;
    procedure SetPartRequestTime(PartIndex:Word ; Time:Cardinal);
  public
    constructor Create();
    destructor Destroy; override;
    procedure LoadFromStream(Stream:TStream);
    function GetIndexPart(Index:Integer):Integer;// 根据图片序号取得其所在的分割部分。
    procedure writeDataHeader(Stream:TStream);
    function GetPartIndex(Part:Integer;var StartIndex,EndIndex:Integer):Boolean;
    property MD5:String read FMD5;
    property Format:TWZLDATAFormat read FFormat;
    property PartState[index:Integer]:TPartState read GetPartState write SetPartStat;
    property ImageCount : Integer read FImageCount;
    property FileName:String read FFileName write FFileName;
    property PartCount: Integer read FPartCount write FPartCount;
    property PartRequestTime[Index:Word]:Cardinal read GetPartRequestTime write SetPartRequestTime;
  end;

  TEnumImagePartProc = reference to procedure (const FileName:String;Stream : TImageFilePartInfo);
  TEnumFileName = reference to procedure (const FileName:String;const FileMD5:String);
  TMiniResFilePackage = class
  private
    FRoot:String;
    FImagePartHash:TDictionary<String,TImageFilePartInfo>;//图片文件分割哈希表
    FFileMD5 :TDictionary<String,String>; //对应文件的MD5值
    FPassWordMD5:AnsiString;
    FInited:Boolean;
    FCheckPassword:Boolean;
    function EqualPassword(S:String):Boolean;
  public
    constructor Create();
    destructor Destroy; override;
    procedure SetRoot(Path:String);
    //生成打包文件
    function BuildPackage(FileList:TStrings;const DstFileName , Password:String):Boolean;
    //读取打包文件
    procedure LoadPackageFromFile(const FileName:String; const Password:String);
    procedure LoadPackageFromStream(Stream:TStream; const Password:String);
    procedure EachImage(Proc:TEnumImagePartProc);
    procedure EachFileMD5(Proc:TEnumFileName);
    function IncludeImageFile(const FileName:String):TWZLDATAFormat;
    function IncludeFile(const FileName:String):Boolean;
    function IncludeSoundFile(const SoudFileName:String):TSoundFormat;
    function TryGetImagePartInfo(const FileName:string ;var PartInfo:TImageFilePartInfo):Boolean;
    property Inited:Boolean read FInited;
    property CheckPassword :Boolean read FCheckPassword write FCheckPassword;
  end;

function GetImageFileHashKeyName(const FileName:String):String;

implementation

function GetImageFileHashKeyName(const FileName:String):String;
begin
  Result := UpperCase(ChangeFileExt(FileName,''));
end;

function GetRunDir():string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;
{ TMiniResFilePackage }

function TMiniResFilePackage.BuildPackage(FileList:TStrings;const DstFileName , Password:String):Boolean;
var
  ReadFileSteam:TMemoryStream;
  PackageStream:TMemoryStream;
  I:Integer;
  FileName:String;
  Utf8:RawByteString;
  ItemInfo:TMiniResFileInfoItem;
  MiniDataFileCount:Integer;
  CompressStream:TMemoryStream;
  MiniDataFileList:TStringList;
  FileDataPosition:Int64;
  MD5:AnsiString;
  PassWordLen:Word;
begin
  MiniDataFileCount := 0;

  //写入图片打包文件部分。
  MiniDataFileList := TStringList.Create;
  for i := 0 to FileList.Count - 1 do
  begin
    if UpperCase(ExtractFileExt(FileList[i])) = '.MINIDATA' then
      MiniDataFileList.Add(FileList[i]);
  end;

  ReadFileSteam := TMemoryStream.Create;
  PackageStream := TMemoryStream.Create;
  CompressStream := TMemoryStream.Create;
  PackageStream.Write(MiniDataFileCount,SizeOf(MiniDataFileCount));
  for I := 0 to MiniDataFileList.Count - 1 do
  begin
    FileName := StringReplace(MiniDataFileList[i],FRoot,'',[rfReplaceAll]);
    FileName := ExtractFilePath(FileName);
    Delete(FileName,Length(FileName),1);
    FileName := FileName;
    Utf8 := UTF8Encode(FileName);
    if FileExists(MiniDataFileList[i]) then
    begin
      try
        ReadFileSteam.Clear;
        ReadFileSteam.LoadFromFile(MiniDataFileList[i]);
        ItemInfo.FileSize := ReadFileSteam.Size;
        ItemInfo.FileNameSize := Length(Utf8);
        PackageStream.Write(ItemInfo,SizeOf(ItemInfo));
        PackageStream.Write(Utf8[1],ItemInfo.FileNameSize);
        PackageStream.CopyFrom(ReadFileSteam,0);
        Inc(MiniDataFileCount);
      Except

      end;
    end;
  end;
  PackageStream.Position := 0;
  PackageStream.Write(MiniDataFileCount,SizeOf(MiniDataFileCount));
  PackageStream.Position := PackageStream.Size;

  //写入非MiniData 文件
  FileDataPosition := PackageStream.Size;
  PackageStream.Write(MiniDataFileCount,SizeOf(MiniDataFileCount));
  MiniDataFileList.Clear;
  for i := 0 to FileList.Count - 1 do
  begin
    if UpperCase(ExtractFileExt(FileList[i])) = '.Z' then
      MiniDataFileList.Add(FileList[i]);
  end;

  MiniDataFileCount := 0;
  for i := 0 to MiniDataFileList.Count - 1 do
  begin
    FileName := StringReplace(MiniDataFileList[i],FRoot,'',[rfReplaceAll]);
    Delete(FileName,Length(FileName)-1,2);
    Utf8 := UTF8Encode(FileName);
    if FileExists(MiniDataFileList[i]) then
    begin
      try
        ReadFileSteam.Clear;
        ReadFileSteam.LoadFromFile(MiniDataFileList[i]);
        ReadFileSteam.Position := 4;
        SetLength(MD5,32);
        ReadFileSteam.Read(MD5[1],32);

        ItemInfo.FileSize := Length(MD5);
        ItemInfo.FileNameSize := Length(Utf8);
        PackageStream.Write(ItemInfo,SizeOf(ItemInfo));
        PackageStream.Write(Utf8[1],ItemInfo.FileNameSize);
        PackageStream.Write(MD5[1],Length(MD5));
        Inc(MiniDataFileCount);
      Except

      end;
    end;
  end;

  PackageStream.Position := FileDataPosition;
  PackageStream.Write(MiniDataFileCount,SizeOf(MiniDataFileCount));

  PackageStream.Position := PackageStream.Size;
  //写入微端密码
  MD5 := THashMD5.GetHashString(Password);
  MD5 := uEDCode.EncodeSource(MD5);
  PassWordLen := Length(MD5);
  PackageStream.Write(PassWordLen,SizeOf(PassWordLen)); //两个字节长度
  PackageStream.Write(MD5[1],Length(MD5));

  PackageStream.Position := 0;

  CompressStream.Position := 0;
  ZCompressStream(PackageStream,CompressStream,zcMax);

  PackageStream.Free;
  CompressStream.Position := 0;
  CompressStream.SaveToFile(DstFileName);
  CompressStream.Free;
  ReadFileSteam.Free;
end;

constructor TMiniResFilePackage.Create;
begin
  FImagePartHash := TDictionary<String,TImageFilePartInfo>.Create;
  FFileMD5 := TDictionary<String,String>.Create;
  FCheckPassword := True;
end;

destructor TMiniResFilePackage.Destroy;
var
  ImagePartArray : TArray<TPair<String,TImageFilePartInfo>>;
  I : Integer;
begin
  FFileMD5.Free;
  ImagePartArray := FImagePartHash.ToArray;
  for i := Low(ImagePartArray) to High(ImagePartArray) do
  begin
    ImagePartArray[i].Value.Free;
  end;
  FImagePartHash.Free;
  inherited;
end;

procedure TMiniResFilePackage.EachFileMD5(Proc: TEnumFileName);
var
 Enum:TEnumerator<TPair<String,String>>;
begin
  Enum := FFileMD5.GetEnumerator;
  while Enum.MoveNext do
    Proc(Enum.Current.Key,Enum.Current.Value);
end;

procedure TMiniResFilePackage.EachImage(Proc: TEnumImagePartProc);
var
 Enum:TEnumerator<TPair<String,TImageFilePartInfo>>;
 FileName:String;
begin
  Enum := FImagePartHash.GetEnumerator;
  Try
    while Enum.MoveNext do
    begin
        Proc(Enum.Current.Value.FileName,Enum.Current.Value);
    end;
  Finally
    Enum.Free;
  End;
end;

procedure TMiniResFilePackage.LoadPackageFromFile(const FileName:String; const Password:String);
var
  FileStream:TMemoryStream;
begin
  if FileExists(FileName) then
  begin
    FileStream := TMemoryStream.Create;
    Try
      FileStream.LoadFromFile(FileName);
      LoadPackageFromStream(FileStream,Password);
    Finally
      FileStream.Free;
    End;
  end;
end;

procedure TMiniResFilePackage.LoadPackageFromStream(Stream: TStream;
  const Password: String);
var
  ZStream , FileStream, BinStream:TMemoryStream;
  FileCount:Integer;
  i: Integer;
  ItemInfo:TMiniResFileInfoItem;
  PartInfo :TImageFilePartInfo;
  Utf8:RawByteString;
  RealFileName:String;
  MD5:AnsiString;
  nPasswordLen:Word;
begin
  FileStream := TMemoryStream.Create;
  Try
    ZDecompressStream(Stream,FileStream);

    FileStream.Position := 0;
    FileStream.Read(FileCount,SizeOf(FileCount));
    for i := 0 to FileCount - 1 do
    begin
      FileStream.Read(ItemInfo,SizeOf(ItemInfo));
      SetLength(Utf8,ItemInfo.FileNameSize);
      FileStream.Read(Utf8[1],ItemInfo.FileNameSize);
      BinStream := TMemoryStream.Create;
      Try
        BinStream.CopyFrom(FileStream,ItemInfo.FileSize);
        PartInfo := TImageFilePartInfo.Create;
        PartInfo.LoadFromStream(BinStream);
        RealFileName := UTF8ToString(Utf8);
        PartInfo.FileName := RealFileName;
        FImagePartHash.AddOrSetValue(GetImageFileHashKeyName(RealFileName),PartInfo);
      Finally
        BinStream.Free;
      End;
      Utf8 := '';
      RealFileName := '';
      MD5 := '';
    end;

    FileStream.Read(FileCount,SizeOf(FileCount));
    for i := 0 to FileCount - 1 do
    begin
      FileStream.Read(ItemInfo,SizeOf(ItemInfo));
      SetLength(Utf8,ItemInfo.FileNameSize);
      FileStream.Read(Utf8[1],ItemInfo.FileNameSize);
      RealFileName := UTF8ToString(Utf8);
      SetLength(MD5,ItemInfo.FileSize);
      FileStream.Read(MD5[1],ItemInfo.FileSize);
      RealFileName := UpperCase(RealFileName);
      FFileMD5.AddOrSetValue(RealFileName,Md5);
      Utf8 := '';
      RealFileName := '';
      MD5 := '';
    end;

    FileStream.Read(nPasswordLen,SizeOf(nPasswordLen));
    SetLength(FPassWordMD5,nPasswordLen);
    FileStream.Read(FPassWordMD5[1],nPasswordLen);
    FPassWordMD5 := uEDCode.DecodeSource(FPassWordMD5);

    EachImage(procedure (const FileName:String;Stream : TImageFilePartInfo)
    begin
      FFileMD5.Add(FileName,Stream.MD5);
    end);

    FInited := True;
    if not EqualPassword(Password) then
    begin
      FFileMD5.Clear;
      FImagePartHash.Clear;
      FInited := False;
    end;
  Finally
    FileStream.Free;
  End;
end;

function TMiniResFilePackage.EqualPassword(S: String): Boolean;
begin
  if FCheckPassword then
  begin
    if THashMD5.GetHashString(S) = string(FPassWordMD5) then
    begin
      Result := True;
    end else
    begin
      Result := False;
    end;
  end else
  begin
    Result := True;
  end;
end;

function TMiniResFilePackage.IncludeFile(const FileName: String): Boolean;
var
  HashFileName:String;
begin
  HashFileName  := StringReplace(FileName,FRoot,'',[rfReplaceAll]);
  HashFileName := UpperCase(HashFileName);
  Result :=  FFileMD5.ContainsKey(HashFileName);
end;

function TMiniResFilePackage.IncludeImageFile(const FileName: String): TWZLDATAFormat;
var
  HashFileName:String;
  Part : TImageFilePartInfo;
begin
  Result := wNone;
  HashFileName  := StringReplace(FileName,FRoot,'',[rfReplaceAll]);
  HashFileName := ChangeFileExt(UpperCase(HashFileName),'');
  if FImagePartHash.TryGetValue(HashFileName,Part) then
  begin
    Result := Part.Format;
  end;
end;

function TMiniResFilePackage.IncludeSoundFile(
  const SoudFileName: String): TSoundFormat;
var
  HashFileName:String;
  Part : TImageFilePartInfo;
begin
  Result := sfNone;
  HashFileName  := StringReplace(SoudFileName,FRoot,'',[rfReplaceAll]);
  HashFileName := ChangeFileExt(UpperCase(HashFileName),'.WAV');
  if FFileMD5.ContainsKey(HashFileName) then
  begin
    Result := sfWav;
  end else
  begin
    HashFileName := ChangeFileExt(UpperCase(HashFileName),'.MP3');
    if FFileMD5.ContainsKey(HashFileName) then
    begin
      Result := sfMp3;
    end else
    begin
      HashFileName := ChangeFileExt(UpperCase(HashFileName),'.OGG');
      if FFileMD5.ContainsKey(HashFileName) then
      begin
        Result := sfOgg;
      end else
      begin
        Result := sfNone;
      end;
    end
  end;
end;

procedure TMiniResFilePackage.SetRoot(Path: String);
begin
  FRoot := Path;
end;

function TMiniResFilePackage.TryGetImagePartInfo(const FileName: string;
  var PartInfo: TImageFilePartInfo): Boolean;
begin
  Result := FImagePartHash.TryGetValue(GetImageFileHashKeyName(FileName),PartInfo);
end;

{ TImageFilePartInfo }

constructor TImageFilePartInfo.Create;
begin

end;

destructor TImageFilePartInfo.Destroy;
begin
  SetLength(FImageIndexPart,0);
  SetLength(FPartStartEnd,0);
  SetLength(FPartDownState,0);
  inherited;
end;

function TImageFilePartInfo.GetIndexPart(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FImageCount) then
  begin
    Result := FImageIndexPart[Index];
  end else
  begin
    Result := 0;
  end;
end;

function TImageFilePartInfo.GetPartIndex(Part: Integer; var StartIndex,
  EndIndex: Integer): Boolean;
begin
  if (Part >= 0) and (Part < FPartCount ) then
  begin
    StartIndex := FPartStartEnd[Part].StartIndex;
    EndIndex := FPartStartEnd[Part].EndIndex;
    Result := True;
  end else
  begin
    Result := False;
  end;
end;

function TImageFilePartInfo.GetPartState(index: Integer): TPartState;
begin
  if (Index >= 0) and (Index < FPartCount) then
    Result := FPartDownState[Index]
  else
    Result := psError;
end;

procedure TImageFilePartInfo.LoadFromStream(Stream: TStream);
var
  ImageCount :Integer;
  PartCount,PartNumber:Integer;
  Flag : Cardinal;
  EndImageIndex:Integer;
  LastIndex,Index:Integer;
  MD5:AnsiString;
begin
  //先解压缩  再解密
  Stream.Position := 0;
  Stream.Read(Flag,SizeOf(Flag));
  if (Flag = MINIWZL_CHECKCODE) or (Flag = MINIDATA_CHECKCODE) then
  begin
    SetLength(MD5,32);
    Stream.Read(MD5[1],32);
    FMD5 := MD5;
    Stream.Read(ImageCount,SizeOf(ImageCount));
    Stream.Read(PartCount,SizeOf(PartCount));
    SetLength(FImageIndexPart,ImageCount);
    FImageCount := ImageCount;
    if Flag = MINIDATA_CHECKCODE then
    begin
      FFormat := wDATA;
      Stream.Read(FDataHeader[1],80);
    end else
    begin
      FFormat := wWZL;
    end;

    LastIndex := 0;
    SetLength(FPartStartEnd,PartCount);
    SetLength(FPartDownState,PartCount);
    SetLength(FPartRequestTime,PartCount);

    FPartCount := PartCount;

    for PartNumber := 0 to PartCount - 1 do
    begin
      Stream.Read(EndImageIndex,SizeOf(EndImageIndex));
      for Index := LastIndex to EndImageIndex do
      begin
        FImageIndexPart[Index] := PartNumber;
      end;
      LastIndex := EndImageIndex + 1;

      if PartNumber = 0 then
        FPartStartEnd[0].StartIndex := 0
      else
        FPartStartEnd[PartNumber].StartIndex :=  FPartStartEnd[PartNumber - 1].EndIndex + 1 ;
      FPartStartEnd[PartNumber].EndIndex := EndImageIndex;

    end;
  end;
end;

procedure TImageFilePartInfo.SetPartRequestTime(PartIndex: Word;
  Time: Cardinal);
begin
  if PartIndex < FPartCount then
  begin
    FPartRequestTime[PartIndex] := Time;
  end;
end;

function TImageFilePartInfo.GetPartRequestTime(PartIndex: Word): Cardinal;
begin
  if PartIndex < FPartCount then
  begin
    Result := FPartRequestTime[PartIndex];
  end else
  begin
    Result := GetTickCount;
  end;
end;

procedure TImageFilePartInfo.SetPartStat(index: Integer; value: TPartState);
begin
  if (Index >= 0) and (Index < FPartCount) then
    FPartDownState[Index] :=  value;
end;

procedure TImageFilePartInfo.writeDataHeader(Stream: TStream);
begin
  Stream.Position := 0;
  Stream.Write(FDataHeader[1],80);
end;

end.
