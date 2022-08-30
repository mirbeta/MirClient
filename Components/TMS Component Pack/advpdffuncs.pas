{*************************************************************************}
{ TMS PDF Engine                                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvPDFFuncs;

interface

uses
  Windows, Classes, SysUtils, Dialogs, Graphics, StrUtils, Math,
  JPEG, PngImage, Generics.Collections, ImgList;


const
  FONTSIZE = 11;
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';
  PDF_MAX_GENERATION_NUM = 65535;
  TTFCFP_MAC_PLATFORMID = 1;
  TTFCFP_MS_PLATFORMID = 3;
  TTFCFP_SYMBOL_CHAR_SET = 0;
  TTFCFP_UNICODE_CHAR_SET = 1;

type
  TWordDynArray = array of word;

  PtrInt = NativeInt;

  PPdfWinAnsiWidth = ^TPdfWinAnsiWidth;

  TPdfWinAnsiWidth = array[#32..#255] of word;

  TUsedWide = array of packed record
      case byte of
      0: (
        Width: word;
        Glyph: word;
      );
      1: (
        Int: Integer;
      );
    end;

  PtrUInt = NativeUInt;

  PPtrUInt = ^PtrUInt;
  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[byte] of SmallInt;

  TByteDynArray = array of byte;

  TPDFBulletStyle = (bsNone, bsStar, bsArrow, bsSquare, bsCheck, bsCircle);

  ///Fit: Display entire page with horizontal & vertical magnified to fit
  ///FitH: Display entire page with only horizontal magnified to fit
  ///FitH100: FitH variant where vertical top is 100
  ///FitV: Display entire page with only vertical magnified to fit
  ///FitV100: FitV variant where horizontal left is 100
  TPdfFitOptions = (foFit, foFitH, foFitH100, foFitV, foFitV100);

  PDFString = AnsiString;

  TSortedWordArray = record
  public
    Values: TWordDynArray;
    Count: Integer;
    /// add a value into the sorted array
    /// - return the index of the new inserted value into the Values[] array
    /// - return -(foundindex+1) if this value is already in the Values[] array
    function Add(aValue: Word): PtrInt;
    /// return the index if the supplied value in the Values[] array
    /// - return -1 if not found
    function IndexOf(aValue: Word): PtrInt; {$ifdef HASINLINE}inline;{$endif}
  end;

  TImageRef = record
    Image: TMemoryStream;
    ImageFileName: string;
  end;

  TPDFHeader = record
    PageNumberFont: TFont;
    PageNumberHAlign: TAlignment;
    PageNumberVAlign: TVerticalAlignment;
    Text: string;
    TextFont: TFont;
    TextHAlign: TAlignment;
    TextVAlign: TVerticalAlignment;
  end;


function RGBA(r, g, b, a: cardinal): COLORREF;
procedure SwapBuffer(P: PWordArray; PLen: Integer);
function GetTTFData(aDC: HDC; aTableName: PAnsiChar; var Ref: TWordDynArray): pointer;
function PNGToJPG(PNGPic: string; Quality: Integer = 100): TJpegImage;
function BMPtoJPG(BMPPic: string; Quality: Integer = 100): TJpegImage;
function GetJpegSize(jpeg: TMemoryStream; out width, height, BitDepth: Integer): Boolean;
function GetBit(const Bits; aIndex: PtrInt): Boolean;
function FastLocateWordSorted(P: PWordArray; R: Integer; Value: word): PtrInt;
function StripSpaces(s: string): string;

implementation

function RGBA(r, g, b, a: cardinal): COLORREF; {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ((r shr 8) or ((g shr 8) shl 8) or ((b shr 8) shl 16) or ((a shr 8) shl 24));
end;

procedure SwapBuffer(P: PWordArray; PLen: Integer);
var
  i: Integer;
begin
  for i := 0 to PLen-1 do
    P^[i] := swap(P^[i]);
end;

function GetTTFData(aDC: HDC; aTableName: PAnsiChar; var Ref: TWordDynArray): pointer;
var L: cardinal;
begin
  result := nil;
  L := GetFontData(aDC,PCardinal(aTableName)^,0,nil,0);
  if L=GDI_ERROR then
    exit;
  SetLength(ref,L shr 1+1);
  if GetFontData(aDC,PCardinal(aTableName)^,0,pointer(ref),L)=GDI_ERROR then
    exit;
  result := pointer(ref);
  SwapBuffer(Result,L shr 1);
end;

function PNGToJPG(PNGPic: string; Quality: Integer = 100): TJpegImage;
var
  PNG: TPNGImage;
  bmp: TBitmap;
begin
  PNG := TPNGImage.Create;
  bmp := TBitmap.Create;
  try
    PNG.LoadFromFile(PNGPic) ;
    bmp.Width := png.Width;
    bmp.Height := png.Height;
    bmp.Canvas.Draw(0,0,png);

    Result := TJpegImage.Create;
    try
      Result.CompressionQuality := Quality;
      Result.Assign(bmp);
    finally
    end;
  finally
    bmp.Free;
    PNG.Free
  end;
end;

function BMPtoJPG(BMPPic: string; Quality: Integer = 100): TJpegImage;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile(BMPPic) ;
    Result := TJpegImage.Create;
    try
      Result.CompressionQuality := Quality;
      Result.Assign(Bitmap);
    finally
    end;
  finally
    Bitmap.Free
  end;
end;

function GetJpegSize(jpeg: TMemoryStream; out width, height, BitDepth: Integer): Boolean;
var
  n: Integer;
  b: byte;
  w: Word;
begin
  Result := false;
  n := jpeg.Size - 8;
  jpeg.Position := 0;

  if n <= 0 then
    Exit;

  jpeg.Read(w,2);
  if w <> $D8FF then
    Exit; // invalid format

  jpeg.Read(b,1);
  while (jpeg.Position<n) and (b=$FF) do begin
    jpeg.Read(b,1);
    case b of
      $C0..$C3: begin
        jpeg.Seek(3,soFromCurrent);
        jpeg.Read(w,2);
        height := swap(w);
        jpeg.Read(w,2);
        width := swap(w);
        jpeg.Read(b,1);
        BitDepth := b*8;
        Result := true; // JPEG format OK
        exit;
      end;
      $FF:
        jpeg.Read(b,1);
      $D0..$D9, $01: begin
        jpeg.Seek(1,soFromCurrent);
        jpeg.Read(b,1);
      end;
      else begin
        jpeg.Read(w,2);
        jpeg.Seek(swap(w)-2, soFromCurrent);
        jpeg.Read(b,1);
      end;
    end;
  end;
end;

function GetBit(const Bits; aIndex: PtrInt): Boolean;
begin
{$ifdef CPU64}
  result := PInt64Array(@Bits)^[aIndex shr 6] and (Int64(1) shl (aIndex and 63)) <> 0;
{$else}
  result := PIntegerArray(@Bits)^[aIndex shr 5] and (1 shl (aIndex and 31)) <> 0;
{$endif}
end;

function FastLocateWordSorted(P: PWordArray; R: Integer; Value: word): PtrInt;
var L,cmp: PtrInt;
begin
  if R<0 then
    result := 0 else begin
    L := 0;
    repeat
      result := (L + R) shr 1;
      cmp := P^[result]-Value;
      if cmp=0 then begin
        result := -result-1; // return -(foundindex+1) if already exists
        exit;
      end;
      if cmp<0 then
        L := result + 1 else
        R := result - 1;
    until (L > R);
    while (result>=0) and (P^[result]>=Value) do dec(result);
    result := result+1; // return the index where to insert
  end;
end;

function StripSpaces(s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(s) do
    if s[i] <> ' ' then
      Result := Result + s[i];
end;

{TSortedWordArray}
function TSortedWordArray.Add(aValue: Word): PtrInt;
begin
  result := FastLocateWordSorted(pointer(Values),Count-1,aValue);
  if result<0 then // aValue already exists in Values[] -> fails
    exit;
  if Count=length(Values) then
    SetLength(Values,Count+100);
  if result<Count then
    move(Values[result],Values[result+1],(Count-result)*2) else
    result := Count;
  Values[result] := aValue;
  inc(Count);
end;

function TSortedWordArray.IndexOf(aValue: Word): PtrInt;
var L,R: PtrInt;
    cmp: Integer;
begin
  Result := 0 shr 1;
  L := 0;
  R := Count-1;
  if 0<=R then
  repeat
    result := (L + R) shr 1;
    cmp := Values[result]-aValue;
    if cmp=0 then
      exit else
    if cmp<0 then
      L := result + 1 else
      R := result - 1;
  until (L > R);
  result := result;
end;

end.
