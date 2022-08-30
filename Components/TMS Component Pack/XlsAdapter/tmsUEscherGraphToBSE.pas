unit tmsUEscherGraphToBSE;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses Sysutils, Classes, tmsXlsMessages, tmsUEscherRecords, tmsUXlsMD5, tmsUFlxMessages;
{************************************************************************************}
{**} function ConvertGraphicToBSE(const Data: ByteArray; const DataType: TXlsImgTypes;
       const DwgGroupCache: PEscherDwgGroupCache; const DwgCache: PEscherDwgCache):TEscherBSERecord ;

      procedure LoadDataWMF(const Data: ByteArray; const DataType: TXlsImgTypes; const BlipData: TStream);

{************************************************************************************}

implementation
{$IFDEF USEPNGLIB}
  uses
  
    {$IFDEF DELPHI2008UP}
        zlib;   //This changed in Delphi 2009 again. Now this units are included in delphi
    {$ELSE}
        flxzlibpas;
    
  {$ENDIF}
{$ENDIF}


type
  TBSEHeader= packed record
   btWin32:        byte;                  // Required type on Win32
   btMacOS:        byte;                  // Required type on Mac
   rgbUid:         TMd5Digest;            // Identifier of blip
   tag:            word;                  // currently unused
   size:           LongWord;              // Blip size in stream
   cRef:           LongWord;              // Reference count on the blip
   foDelay:        LongWord;              // File offset in the delay stream
   usage:          byte;                  // How this blip is used (MSOBLIPUSAGE)
   cbName:         byte;                  // length of the blip name
   unused2:        byte;                  // for the future
   unused3:        byte;                  // for the future
  end;

  TWMFBlipHeader = packed record
    m_rgbUid: TMd5Digest;  { The secondary, or data, UID - should always be set. }

    { Metafile Blip overhead = 34 bytes. m_cb gives the number of
     bytes required to store an uncompressed version of the file, m_cbSave
     is the compressed size.  m_mfBounds gives the boundary of all the
     drawing calls within the metafile (this may just be the bounding box
     or it may allow some whitespace, for a WMF this comes from the
     SetWindowOrg and SetWindowExt records of the metafile). }
    m_cb: Longint;           // Cache of the metafile size
    m_rcBounds: Array[0..3] of Longint;     // Boundary of metafile drawing commands
    m_ptSize: Array[0..1] of Longint;       // Size of metafile in EMUs
    m_cbSave: Longint;       // Cache of saved size (size of m_pvBits)
    m_fCompression: byte; // MSOBLIPCOMPRESSION
    m_fFilter: byte;      // always msofilterNone
  end;

procedure LoadDataBMP(const Data: ByteArray; const DataType: TXlsImgTypes; const BlipData: TStream);
var
  Tag: byte;
begin
  Tag:=$FF;
  BlipData.Write(Tag, SizeOf(Tag));
  BlipData.Write(Data[0], Length(Data));
end;

{$IFDEF USEPNGLIB}
procedure XlsMetafilesToXls(const ImgData: ByteArray; const OutStream: TStream; const IsEMF: Boolean);
const
  Z_OK=0;
  Z_STREAM_END=1;
var
  HeadOfs: LongWord;
  ZL: TZStreamRec;
  Buff: ByteArray;
  Res: integer;
begin
  HeadOfs := 0;
  if (not IsEMF and (Length(ImgData) > 4)) and (ImgData[0] = $D7) and (ImgData[1] = $CD) and (ImgData[2] = $C6) and (ImgData[3] = $9A) then
    HeadOfs:= HeadOfs + 22;

  //compress the image
   FillChar(Zl, SizeOf(Zl), 0);
   Zl.next_in:= @ImgData[HeadOfs];
   Zl.avail_in:= Length(ImgData) - Integer(HeadOfs);
   SetLength(Buff, 2048);     //Arbitrary block size
   Zl.next_out:=@Buff[0];
   Zl.avail_out:=Length(Buff);

   if DeflateInit_(ZL, 9, zlib_version, SIZEOF(TZStreamRec))<> Z_OK then
      raise Exception.Create(ErrInvalidWmf);
   while Zl.avail_in > 0 do
   begin
     Res:= deflate(ZL, Z_NO_FLUSH);
     if (Res<> Z_OK) then
       raise Exception.Create(ErrInvalidWmf);
     OutStream.Write(Buff[0], Length(Buff) - Integer(Zl.avail_out)) ;
     Zl.next_out:=@Buff[0];
     Zl.avail_out:=Length(Buff);
   end;

   Zl.next_in := nil;
   Zl.avail_in := 0;
   while deflate(ZL, Z_FINISH) <>Z_STREAM_END do
   begin
     OutStream.Write(Buff[0], Length(Buff) - Integer(Zl.avail_out));
     Zl.next_out:=@Buff[0];
     Zl.avail_out:=Length(Buff);
   end;

   if (Zl.avail_out > 0) then
   begin
     OutStream.Write(Buff[0], Length(Buff) - Integer(Zl.avail_out))
   end;

   Res := deflateEnd(ZL);
   if (Res<> Z_OK) then
     raise Exception.Create(ErrInvalidWmf);

end;
{$ELSE}
procedure XlsMetafilesToXls(const ImgData: ByteArray; const OutStream: TStream; const IsEMF: Boolean);
begin
  raise Exception.Create('To insert WMF files you need to instal TPngImage');
end;
{$ENDIF}

type
  Int32Rec = record
     Value: Longint;
  end;

  PInt32Rec = ^Int32Rec;
procedure LoadDataWMF(const Data: ByteArray; const DataType: TXlsImgTypes; const BlipData: TStream);
var
  cb: LongWord;
  ptSize: Array of byte;
  OtherDat: Array of byte;
  StreamPos: Int64;
  Zero: word;
  WidthEMU, HeightEMU: integer;
begin
  Zero := 0;
  cb := length(Data);
  BlipData.Write(cb, sizeof(cb));
  if DataType in [xli_Wmf] then
  begin
    BlipData.Write(Data[6],1);BlipData.Write(Data[7],1); BlipData.Write(Zero,2);
    BlipData.Write(Data[8],1);BlipData.Write(Data[9],1); BlipData.Write(Zero,2);
    BlipData.Write(Data[10],1);BlipData.Write(Data[11],1);BlipData.Write(Zero,2);
    BlipData.Write(Data[12],1);BlipData.Write(Data[13],1);BlipData.Write(Zero,2);

    SetLength (ptSize, 8);
    ptSize[0] := 24;
    ptSize[1] := 240;
    ptSize[2] := 255;
    ptSize[3] := 0;
    ptSize[4] := 24;
    ptSize[5] := 240;
    ptSize[6] := 255;
    ptSize[7] := 0;
    BlipData.Write(ptSize[0], Length(ptSize));
  end
  else
  begin
    BlipData.Write(Data[8], 16);
    WidthEMU := (PInt32Rec(@Data[24 + 8]).Value - PInt32Rec(@Data[24]).Value) * 360;
    HeightEMU := (PInt32Rec(@Data[24 + 12]).Value - PInt32Rec(@Data[24 + 4]).Value) * 360;
    BlipData.Write(WidthEmu, SizeOf(WidthEMU));
    BlipData.Write(HeightEmu, SizeOf(HeightEMU));
  end;

  SetLength (OtherDat, 6);
  OtherDat[0] := 0;
  OtherDat[1] := 0;
  OtherDat[2] := 0;
  OtherDat[3] := 0;
  OtherDat[4] := 0;
  OtherDat[5] := 254;
  StreamPos := BlipData.Position;
  BlipData.Write(OtherDat[0], 6);

  if DataType = xli_Emf then
    XlsMetafilesToXls(Data, BlipData, true) else
    XlsMetafilesToXls(Data, BlipData, false);

  BlipData.Position := StreamPos;
  cb := LongWord(BlipData.Size-StreamPos-6);
  BlipData.Write(cb, sizeof(cb));
  BlipData.Position := BlipData.Size;
end;

{************************************************************************************}
function ConvertGraphicToBSE(const Data: ByteArray; const DataType: TXlsImgTypes;
  const DwgGroupCache: PEscherDwgGroupCache; const DwgCache: PEscherDwgCache):TEscherBSERecord;
var
  Eh: TEscherRecordHeader;
  BSEHeader: TBSEHeader;
  Md5Stream: TMd5Stream;
  BlipData: TMemoryStream;
  BlipHeader: TEscherRecordHeader;
begin
  FillChar(BSEHeader, SizeOf(BSEHeader), 0);
  Md5Stream:=TMd5Stream.Create;
  try
    Md5Stream.Write(Data[0], Length(Data));
    BSEHeader.rgbUid:= Md5Stream.GetDigest;
  finally
    FreeAndNil(Md5Stream);
  end;

//  FillChar(BSEHeader, SizeOf(BSEHeader), 0);
  BlipData:=TMemoryStream.Create;
  try
    //Common header
    BlipData.Write(BSEHeader.rgbUid, SizeOf(BSEHeader.rgbUid));

    // Specific info
    if DataType in [xli_JPEG, xli_PNG, xli_BMP] then
      LoadDataBMP(Data, DataType, BlipData) else
      LoadDataWMF(Data, DataType, BlipData);

    BSEHeader.btWin32:= XlsImgConv[DataType];
    BSEHeader.btMacOS:= msoblipPICT;

    BSEHeader.tag:=$FF;
    BSEHeader.size:= BlipData.Size+ SizeOf(BlipHeader);
    BSEHeader.cRef:=0;
    BSEHeader.foDelay:=0;

    Eh.Id:= MsofbtBSE;
    Eh.Pre:=2 + XlsImgConv[DataType] shl 4;
    Eh.Size:=BSEHeader.size + SizeOf(BSEHeader);
    Result:= TEscherBSERecord.Create(Eh, DwgGroupCache, DwgCache, DwgGroupCache.BStore);

    BlipHeader.Id:= XlsBlipHeaderConv[DataType];
    BlipHeader.Pre:= XlsBlipSignConv[DataType] shl 4;
    BlipHeader.Size:=BlipData.Size;

    BlipData.Position:=0;
    Result.CopyFromData(@BSEHeader, BlipHeader, BlipData);
  finally
    FreeAndNil(BlipData);
  end; //finally
end;

end.
