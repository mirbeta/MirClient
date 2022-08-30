/// Utility methods to convert images form file formats to Excel internal formats.
unit tmsUXlsPictures;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses
  {$IFDEF FLX_VCL}
   {$IFNDEF FIREMONKEY}
    Windows, Graphics,
    {$IFDEF FLX_NEEDSJPEG} JPEG, {$ENDIF}

    {$INCLUDE UsePngLib.inc}
   {$ELSE}
   FMX.Types,
   {$ENDIF}
  {$ENDIF}

  {$IFDEF FLX_CLX}
    Qt, QGraphics, QGrids, Types, QControls,
  {$ENDIF}
  {$IFDEF FLX_NEEDSVARIANTS} variants,{$ENDIF} //Delphi 6 or above

  SysUtils, Classes, tmsUFlxMessages, tmsUExcelAdapter;

  type
  /// <summary>
  /// This record is for internal use.
  /// </summary>                      
  TSmallRect=packed record
    /// <summary>Internal use. </summary>
    Left: SmallInt;

    /// <summary>Internal use. </summary>
    Top: SmallInt;

    /// <summary>Internal use. </summary>
    Right: SmallInt;

    /// <summary>Internal use. </summary>
    Bottom: SmallInt;
  end;

  /// <summary>WMF Header. Internal use. </summary>
  TMetafileHeader = packed record
    /// <summary>Internal use. </summary>
    Key: Longint;

    /// <summary>Internal use. </summary>
    Handle: SmallInt;

    /// <summary>Internal use. </summary>
    Rect: TSmallRect;

    /// <summary>Internal use. </summary>
    Inch: Word;

    /// <summary>Internal use. </summary>
    Reserved: Longint;

    /// <summary>Internal use. </summary>
    CheckSum: Word;
  end;

{$IFNDEF FIREMONKEY}

//------------------------------------------------------------------------------
  /// <summary>
  /// This method will load a WMF image returned from Excel into a TPicture image.
  /// </summary>
  /// <remarks>
  /// WMF images are stored differently in Excel than in disk, so you need this method to do the
  /// conversion.<para></para>
  /// <para></para>
  /// Normally you will want to use SaveImgStreamToGraphic or SaveImgStreamToDiskImage instead of this
  /// method, since those ones convert any kind of images form Excel to their disk representation, not only
  /// metafiles.
  /// </remarks>
  /// <param name="OutPicture">TPicture where the image will be loaded.</param>
  /// <param name="InStream">Stream with the image as Excel returns it.</param>
  /// <param name="PicType">Picture type as returned from Excel. this method will only handle EMF and
  ///                       WMF, for a generic method that an convert any kind of images use
  ///                       SaveImgStreamToGraphic.</param>                                                
  procedure LoadWmf(const OutPicture: TPicture; const InStream: TStream; const PicType: TXlsImgTypes);

  /// <summary>
  /// Loads an image returned by <see cref="TFlexCelImport.GetPicture@integer@TStream@TXlsImgTypes@TClientAnchor" text="TFlexCelImport.GetPicture" />
  /// into a TPicture object.
  /// </summary>
  /// <remarks>
  /// If you want to save the image to disk instead of loading it into a TPicture, you might want to use <see cref="SaveImgStreamToDiskImage@TStream@TXlsImgTypes@TStream@boolean" text="SaveImgStreamToDiskImage" />
  /// instead.
  /// </remarks>
  /// <param name="Pic">Stream with the data returned by <see cref="TFlexCelImport.GetPicture@integer@TStream@TXlsImgTypes@TClientAnchor" text="TFlexCelImport.GetPicture" />.</param>
  /// <param name="PicType">Picture type returned by <see cref="TFlexCelImport.GetPicture@integer@TStream@TXlsImgTypes@TClientAnchor" text="TFlexCelImport.GetPicture" />.</param>
  /// <param name="Picture">TPicture object where you want to load the image.</param>
  /// <param name="Handled">Will return True if the image could be loaded, false if it couldn't be parsed.</param>
  procedure SaveImgStreamToGraphic(const Pic: TStream; const PicType: TXlsImgTypes; const Picture: TPicture; out Handled: boolean);
{$ENDIF}

  /// <summary>
  /// Converts an image returned by <see cref="TFlexCelImport.GetPicture@integer@TStream@TXlsImgTypes@TClientAnchor" text="TFlexCelImport.GetPicture" />
  /// from Excel internal format to a format that can be saved to disk.
  /// </summary>
  /// <remarks>
  /// If you want to load the image into a TPicture object instead of saving it to disk, you might want to
  /// use <see cref="SaveImgStreamToGraphic@TStream@TXlsImgTypes@TPicture@boolean" text="SaveImgStreamToGraphic" />
  /// instead.
  /// </remarks>
  /// <param name="Pic">Stream with the data returned by <see cref="TFlexCelImport.GetPicture@integer@TStream@TXlsImgTypes@TClientAnchor" text="TFlexCelImport.GetPicture" />.</param>
  /// <param name="PicType">Picture type returned by <see cref="TFlexCelImport.GetPicture@integer@TStream@TXlsImgTypes@TClientAnchor" text="TFlexCelImport.GetPicture" />.</param>
  /// <param name="OutStream">Stream where you want to save the image.</param>
  /// <param name="Saved">Will return true if FlexCel coud process the file, false if the file was in
  ///                     an unknown format.</param>                                                                                                                                  
  procedure SaveImgStreamToDiskImage(const Pic: TStream; const PicType: TXlsImgTypes; const OutStream: TStream; out Saved: boolean);

  /// <summary>
  /// \Returns a bitmap containing the pattern specified.
  /// </summary>
  /// <remarks>
  /// You will normally not need to use this method. It is used internally by FlexCelGrid to display bitmap
  /// patterns in cells.<para></para>
  /// <para></para>
  /// This method creates a 4x4 or 8x4 bitmap with the pattern number specified by n, using ColorFg as the
  /// foreground color and ColorBg as the background color for the pattern.<para></para>
  /// <para></para>
  /// You can use the returned bitmap as bitmap for a TCanvas.Brush<para></para>
  /// <para></para>
  /// It is your responsibility to free the created bitmap when it is not more in use.<para></para>
  /// <para></para>
  /// Possible n values:<para></para>
  /// <img name="patterns" /><para></para>
  /// <para></para>
  /// n=1 means no background.<para></para>
  /// 
  /// </remarks>
  /// <param name="n">Indicates the type of pattern, as in the image above.</param>
  /// <param name="ColorFg">Color for the foreground pattern.</param>
  /// <param name="ColorBg">Color for the background pattern.</param>
  /// <returns>
  /// \ \  
  /// </returns>                                                                                           
  function CreateBmpPattern(const n, ColorFg, ColorBg: integer): TBitmap;

  /// <summary>
  /// Computes the Aldus Checksum for a Windows Metafile.
  /// </summary>
  /// <remarks>
  /// This method is for internal use.
  /// </remarks>
  /// <param name="WMF">Header of the metafile.</param>
  function ComputeAldusChecksum(var WMF: TMetafileHeader): Word;

//------------------------------------------------------------------------------

implementation
function ComputeAldusChecksum(var WMF: TMetafileHeader): Word;
type
  PWord = ^Word;
var
  pW: PWord;
  pEnd: PWord;
begin
  Result := 0;
  pW := @WMF;
  pEnd := @WMF.CheckSum;
  while PAddress(pW) < PAddress(pEnd) do
  begin
    Result := Result xor pW^;
    Inc(PAddress(pW), SizeOf(Word));
  end;
end;

{$IFDEF USEPNGLIB}
procedure LoadWmfInStream(const OutStream: TStream; const InStream: TStream; const PicType: TXlsImgTypes; out Saved: boolean);
const
  Z_OK=0;
  Z_STREAM_END=1;
var
  WmfHead: TMetafileHeader;
  CompressedStream: TMemoryStream;
  ZL: TZStreamRec;
  Buff: Array of byte;
  Res, LastOut: integer;
  BoundRect: TRect;
  IsCompressed: byte;
begin
    Saved:=true;
    if PicType=xli_wmf then
    begin
      //Write Metafile Header
      FillChar(WmfHead, SizeOf(WmfHead), 0);
      WmfHead.Key:=Integer($9AC6CDD7);
      InStream.Position:=4;

      //We can't just read into WmfHead.Rect, because this is small ints, not ints
      InStream.ReadBuffer(BoundRect, SizeOf(BoundRect));
      WmfHead.Rect.Left:=BoundRect.Left;
      WmfHead.Rect.Top:=BoundRect.Top;
      WmfHead.Rect.Right:=BoundRect.Right;
      WmfHead.Rect.Bottom:=BoundRect.Bottom;

      WmfHead.Inch:=96;
      WmfHead.CheckSum:=ComputeAldusChecksum(WmfHead);
      OutStream.WriteBuffer(WmfHead, SizeOf(WmfHead));
    end;

    InStream.Position:=32;
    InStream.ReadBuffer(IsCompressed, SizeOf(IsCompressed));
    InStream.Position:=34;

    if IsCompressed=0 then //Data is compressed
    begin
      //Uncompress Data
      Fillchar(ZL, SIZEOF(TZStreamRec), 0);

      CompressedStream:=TMemoryStream.Create;
      try
        CompressedStream.CopyFrom(InStream, InStream.Size- InStream.Position);
        CompressedStream.Position:=0;
        FillChar(Zl, SizeOf(Zl), #0);
        Zl.next_in:=CompressedStream.Memory;
        Zl.avail_in:=CompressedStream.Size;
        SetLength(Buff, 2048);     //Arbitrary block size
        Zl.next_out:=@Buff[0];
        Zl.avail_out:=Length(Buff);
        LastOut:=0;
        try
          if InflateInit_(ZL, zlib_version, SIZEOF(TZStreamRec))<> Z_OK then
            raise Exception.Create(ErrInvalidWmf);
          repeat
            Res:=Inflate(ZL,0);
            if (Res<> Z_OK) and (Res<>Z_STREAM_END) then
              raise Exception.Create(ErrInvalidWmf);

            OutStream.WriteBuffer(Buff[0], Integer(Zl.Total_Out) - LastOut);
            LastOut:=Zl.Total_Out;
            Zl.next_out:=@Buff[0];
            Zl.avail_out:=Length(Buff);
          until Res= Z_STREAM_END;
        finally
          InflateEnd(ZL);
        end; //Finally
      finally
        FreeAndNil(CompressedStream);
      end;
    end else
    begin
      OutStream.CopyFrom(InStream, InStream.Size-InStream.Position);
    end;
end;

procedure LoadWmf(const OutPicture: TPicture; const InStream: TStream; const PicType: TXlsImgTypes);
var
  MemStream: TMemoryStream;
  Saved: boolean;
begin
  MemStream:=TMemoryStream.Create;
  try
    LoadWmfInStream(MemStream, InStream, PicType, Saved);
    MemStream.Position:=0;
    OutPicture.Graphic.LoadFromStream(MemStream);
  finally
    FreeAndNil(MemStream);
  end; //Finally
end;

{$ELSE}
procedure LoadWmfInStream(const OutStream: TStream; const InStream: TStream; const PicType: TXlsImgTypes; out Saved: boolean);
begin
  Saved := false;
end;

{$IFNDEF FIREMONKEY}
procedure LoadWmf(const OutPicture: TPicture; const InStream: TStream; const PicType: TXlsImgTypes);
begin
end;

{$ENDIF}
{$ENDIF}

{$IFNDEF FIREMONKEY}
procedure SaveImgStreamToGraphic(const Pic: TStream; const PicType: TXlsImgTypes; const Picture: TPicture; out Handled: boolean);
var
  Bmp:TBitmap;
  {$IFDEF FLX_VCL}
  Jpeg: TJpegImage;
  {$ENDIF}
  {$IFDEF USEPNGLIB}
    Png: TPngImage;
    {$IFDEF FLX_SUPPORTSWMF}
      Wmf: TMetafile;
    {$ENDIF}
  {$ENDIF}
begin
  Handled:=true;
  case PicType of
    {$IFDEF FLX_VCL}
       xli_Jpeg:
       begin
         Jpeg:=TJPEGImage.Create;
         try
           Picture.Graphic:=Jpeg;
         finally
           FreeAndNil(Jpeg); //Remember TPicture.Graphic keeps a COPY of the TGraphic
         end;
         (Picture.Graphic as TJPEGImage).Performance:=jpBestQuality;
         Picture.Graphic.LoadFromStream(Pic);
       end;
      xli_Bmp:
      begin
        Bmp:=TBitmap.Create;
        try
          Picture.Graphic:=Bmp;
         finally
           FreeAndNil(Bmp); //Remember TPicture.Graphic keeps a COPY of the TGraphic
         end;
        Picture.Graphic.LoadFromStream(Pic);
      end;
      //There is no direct support for PNG, because there is not a standard Delphi class to support it.
      //No direct support for wmf/emf, because it uses zlib and it would have to be added to the package list.
      //To support it define USEPNGLIB at the top of this file

      {$IFDEF USEPNGLIB}
        xli_png:
        begin
          Png:=TPNGImage.Create;
          try
            Picture.Graphic:=Png;
           finally
             FreeAndNil(Png); //Remember TPicture.Graphic keeps a COPY of the TGraphic
           end;
          Picture.Graphic.LoadFromStream(Pic);
        end;

        {$IFDEF FLX_SUPPORTSWMF}
          xli_wmf, xli_emf:
          begin
            Wmf:=TMetaFile.Create;
            try
              Picture.Graphic:=Wmf;
            finally
              FreeAndNil(Wmf);
            end; //finally
            LoadWmf(Picture, Pic, PicType);
          end;
        {$ENDIF}
      {$ENDIF}

    {$ENDIF}
    {$IFDEF FLX_CLX}
    //Here png is directly supported. Not metafiles...
      xli_Bmp, xli_Jpeg, xli_Png:
      begin
        Bmp:=TBitmap.Create;
        try
          Picture.Graphic:=Bmp;
         finally
           FreeAndNil(Bmp); //Remember TPicture.Graphic keeps a COPY of the TGraphic
         end;
        Picture.Graphic.LoadFromStream(Pic);
      end;
    {$ENDIF}

    else Handled:=False;
  end; //case
end;
{$ENDIF}

procedure SaveImgStreamToDiskImage(const Pic: TStream; const PicType: TXlsImgTypes; const OutStream: TStream; out Saved: boolean);
begin
  Saved := true;
  case PicType of
    xli_Emf,
    xli_Wmf:
      LoadWmfInStream(OutStream, Pic, PicType, Saved);

    xli_Jpeg,
    xli_Png,
    xli_Bmp: OutStream.CopyFrom(Pic, Pic.Size);

    else Saved := false;
  end;
end;



{$IFDEF FLX_CLX}
   {$IFDEF VER140}
       // Kylix3 is 140 too... and it allows patterns. Patterns are not allowed for d6/bcb6 clx.
       {$DEFINE FLX_NOPATTERN}
     
   {$ENDIF}
{$ENDIF}

procedure Fill8x8Image(const Bmp: TBitmap);
begin
{$IFNDEF FIREMONKEY}
  Bmp.Canvas.Draw(0,4,Bmp);
  Bmp.Canvas.Draw(4,0,Bmp);
{$ENDIF}
end;

{$IFDEF FLX_NOPATTERN}
function CreateBmpPattern(const n, ColorFg, ColorBg: integer): TBitmap;
var
  Ac: TCanvas;
begin
  Result:=TBitmap.Create;
  try
    Result.Width:=8;
    Result.Height:=8;
{$IFDEF FLX_CLX}
    Result.PixelFormat:=pf32bit;
{$ELSE}
    Result.PixelFormat := pfDevice; //for win95
{$ENDIF}
    Ac:=Result.Canvas;
    case n of
      1: //No pattern
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,8,8));
        end;
      else //fill pattern     //No pixel support on tcanvas, so we can't use patterns here.
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,8,8));
        end;
    end; //case
  except
    FreeAndNil(Result);
    raise;
  end;
end;
{$ELSE}
function CreateBmpPattern(const n, ColorFg, ColorBg: integer): TBitmap;
{$IFNDEF FIREMONKEY}
var
  Ac: TCanvas;
  x,y: integer;
{$ENDIF}
begin
{$IFDEF FIREMONKEY}
  Result := nil;
{$ELSE}
  Result:=TBitmap.Create;
  try
    Result.Width:=8; //We just need a 4x4 bitmap, but windows95 does not like it.
    Result.Height:=8;
{$IFDEF FLX_CLX}
    Result.PixelFormat:=pf32bit;
{$ELSE}
    Result.PixelFormat := pfDevice; //for win95
{$ENDIF}
    Ac:=Result.Canvas;
    case n of
      1: //No pattern
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,8,8));
        end;
      2: //fill pattern
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,8,8));
        end;
      3: //50%
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,8,8));
          for y:=0 to 7 do
            for x:=0 to 3 do
              Ac.Pixels[x*2+y mod 2,y]:=ColorFg;
        end;
      4: //75%
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorBg;
          Ac.Pixels[2,1]:=ColorBg;
          Ac.Pixels[0,2]:=ColorBg;
          Ac.Pixels[2,3]:=ColorBg;
          Fill8x8Image(Result);
        end;
      5: //25%
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorFg;
          Ac.Pixels[2,1]:=ColorFg;
          Ac.Pixels[0,2]:=ColorFg;
          Ac.Pixels[2,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      6: //Horz lines
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,4,2));
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,2,4,4));
          Fill8x8Image(Result);
        end;
      7: //Vert lines
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,2,4));
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(2,0,4,4));
          Fill8x8Image(Result);
        end;
      8: //   \ lines
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorFg; Ac.Pixels[1,0]:=ColorFg;
          Ac.Pixels[1,1]:=ColorFg; Ac.Pixels[2,1]:=ColorFg;
          Ac.Pixels[2,2]:=ColorFg; Ac.Pixels[3,2]:=ColorFg;
          Ac.Pixels[3,3]:=ColorFg; Ac.Pixels[0,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      9: //   / lines
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[2,0]:=ColorFg; Ac.Pixels[3,0]:=ColorFg;
          Ac.Pixels[1,1]:=ColorFg; Ac.Pixels[2,1]:=ColorFg;
          Ac.Pixels[0,2]:=ColorFg; Ac.Pixels[1,2]:=ColorFg;
          Ac.Pixels[3,3]:=ColorFg; Ac.Pixels[0,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      10: //  diagonal hatch
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorFg; Ac.Pixels[1,0]:=ColorFg;
          Ac.Pixels[0,1]:=ColorFg; Ac.Pixels[1,1]:=ColorFg;
          Ac.Pixels[2,2]:=ColorFg; Ac.Pixels[3,2]:=ColorFg;
          Ac.Pixels[2,3]:=ColorFg; Ac.Pixels[3,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      11: //  bold diagonal
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[2,0]:=ColorBg; Ac.Pixels[3,0]:=ColorBg;
          Ac.Pixels[0,2]:=ColorBg; Ac.Pixels[1,2]:=ColorBg;
          Fill8x8Image(Result);
        end;
      12: //  thin horz lines
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,4,1));
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,1,4,4));
          Fill8x8Image(Result);
        end;
      13: //  thin vert lines
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,1,4));
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(1,0,4,4));
          Fill8x8Image(Result);
        end;
      14: //  thin \ lines
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorFg;
          Ac.Pixels[1,1]:=ColorFg;
          Ac.Pixels[2,2]:=ColorFg;
          Ac.Pixels[3,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      15: //  thin / lines
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[3,0]:=ColorFg;
          Ac.Pixels[2,1]:=ColorFg;
          Ac.Pixels[1,2]:=ColorFg;
          Ac.Pixels[0,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      16: //  thin horz hatch
        begin
          Ac.Brush.Color:=ColorFg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(1,1,4,4));
          Fill8x8Image(Result);
        end;
      17: //  thin diag
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorFg; Ac.Pixels[2,0]:=ColorFg;
          Ac.Pixels[1,1]:=ColorFg;
          Ac.Pixels[0,2]:=ColorFg; Ac.Pixels[2,2]:=ColorFg;
          Ac.Pixels[3,3]:=ColorFg;
          Fill8x8Image(Result);
        end;
      18: //  12.5 %
        begin
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,4,4));
          Ac.Pixels[0,0]:=ColorFg;
          Ac.Pixels[2,2]:=ColorFg;
          Fill8x8Image(Result);
        end;
      19: //  6.25 %
        begin
          //Not needed now. Result.Width:=8;
          Ac.Brush.Color:=ColorBg;
          Ac.FillRect(Rect(0,0,8,8));
          Ac.Pixels[0,0]:=ColorFg;
          Ac.Pixels[4,2]:=ColorFg;
          Ac.Pixels[0,4]:=ColorFg;
          Ac.Pixels[4,6]:=ColorFg;
        end;
    end; //case
  except
    FreeAndNil(Result);
    raise;
  end;
  {$ENDIF}
end;
{$ENDIF}


end.
