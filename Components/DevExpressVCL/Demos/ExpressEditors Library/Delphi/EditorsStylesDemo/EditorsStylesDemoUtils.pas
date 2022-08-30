unit EditorsStylesDemoUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI9}
  Windows,
{$ENDIF}
  JPEG, Graphics;

type
  TJPEGOptions = record
    CompressionQuality: Integer;
    Grayscale: Boolean;
    ProgressiveEncoding: Boolean;
  end;


procedure ConvertBitmapToJPEG(AFileName: String; AReplaceFile: Boolean; AJPEGOptions: TJPEGOptions); overload;
procedure ConvertBitmapToJPEG(ABitmapFileName, AJPGFileName: String; AReplaceFile: Boolean; AJPEGOptions: TJPEGOptions); overload;
procedure ConvertBitmapToJPEG(ABitmap: TBitmap; AJPGFileName: String; AJPEGOptions: TJPEGOptions); overload;
procedure ConvertJPEGToBitmap(AFileName: String; AReplaceFile: Boolean);

implementation

uses Classes, SysUtils, Dialogs;

procedure AssignJPEGProperties(AJPEGImage: TJPEGImage; AJPEGOptions: TJPEGOptions);
begin
  with AJPEGImage do
  begin
    CompressionQuality := AJPEGOptions.CompressionQuality;
    Grayscale := AJPEGOptions.Grayscale;
    ProgressiveEncoding := AJPEGOptions.ProgressiveEncoding;
  end;
end;

procedure ConvertBitmapToJPEG(ABitmap: TBitmap; AJPGFileName: String; AJPEGOptions: TJPEGOptions);
var
  J: TJPEGImage;
begin
  J := TJPEGImage.Create;
  try
    AssignJPEGProperties(J, AJPEGOptions);
    J.Assign(ABitmap);
    J.SaveToFile(AJPGFileName);
  finally
    J.Free;
  end;
end;

procedure ConvertBitmapToJPEG(ABitmapFileName, AJPGFileName: String; AReplaceFile: Boolean; AJPEGOptions: TJPEGOptions);
var
  J: TJPEGImage;
  B: TBitMap;
begin
  B := TBitMap.Create;
  J := TJPEGImage.Create;
  try
    AssignJPEGProperties(J, AJPEGOptions);
    B.LoadFromFile(ABitmapFileName);
    J.Assign(B);
    J.SaveToFile(AJPGFileName);
  finally
    B.Free;
    J.Free;
  end;
end;

procedure ConvertBitmapToJPEG(AFileName: String; AReplaceFile: Boolean; AJPEGOptions: TJPEGOptions);
var
  J: TJPEGImage;
  B: TBitMap;
  AJPGFileName: String;
begin
  B := TBitMap.Create;
  J := TJPEGImage.Create;
  try
    AssignJPEGProperties(J, AJPEGOptions);
    B.LoadFromFile(AFileName);
    J.Assign(B);
    AJPGFileName := ChangeFileExt(AFileName, '.jpg');
    J.SaveToFile(AJPGFileName);
    if AReplaceFile then
      DeleteFile(AFileName);
  finally
    B.Free;
    J.Free;
  end;
end;

procedure ConvertJPEGToBitmap(AFileName: String; AReplaceFile: Boolean);
var
  J: TJPEGImage;
  B: TBitMap;
  ABMPFileName: String;
begin
  J := TJPEGImage.Create;
  B := TBitMap.Create;
  try
    J.LoadFromFile(AFileName);
    B.Assign(J);
    ABMPFileName := ChangeFileExt(AFileName, '.bmp');
    B.SaveToFile(ABMPFileName);
    if AReplaceFile then
      DeleteFile(AFileName);
  finally
    J.Free;
    B.Free;
  end;
end;

end.
