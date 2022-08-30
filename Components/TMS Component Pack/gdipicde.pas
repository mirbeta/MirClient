{**************************************************************************}
{ TAdvGDIPPicture design editor                                            }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2006 - 2013                                                }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit GDIPicDE;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Classes, Controls, Dialogs, ExtDlgs, GDIPicture, Menus,
  DesignIntf, DesignEditors, VCLEditors, ImgList, Graphics, SysUtils, Math;


type
  TGDIPPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TAdvImageIndexProperty = class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
  protected
    function ImageList: TCustomImageList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure ListMeasureWidth(const Value: string;
      ACanvas: TCanvas; var AWidth: Integer); virtual;
    procedure ListMeasureHeight(const Value: string;
      ACanvas: TCanvas; var AHeight: Integer); virtual;
    procedure ListDrawValue(const Value: string;
      ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
  end;

implementation

uses
  TypInfo;

{ TGDIPPictureProperty }

procedure TGDIPPictureProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog := TOpenPictureDialog.Create(nil);

  OpenDialog.Filter := 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.png;*.tiff)|*.jpg;*.jpeg;*.gif;*.bmp;*.png;*.tiff;*.tif|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|PNG files (*.png)|*.png|TIFF files (*.tiff)|*.tiff;*.tif';

  if Opendialog.Execute then
  begin
    TGDIPPicture(GetOrdValue).LoadFromFile(Opendialog.FileName);
    Modified;
  end;
  OpenDialog.Free;
end;

function TGDIPPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TGDIPPictureProperty.GetValue: String;
begin
  if not TGDIPPicture(GetOrdValue).Empty then
    Result := '(TPicture)'
  else
    Result := '(None)';
end;

procedure TGDIPPictureProperty.SetValue(const Value: String);
var
  gdip: TGDIPPicture;
begin
  inherited;
  if (Value = '') then  // picture is cleared
  begin
    gdip := TGDIPPicture(GetOrdValue);
    if Assigned(gdip) then
      gdip.Assign(nil);
  end;
end;


function TAdvImageIndexProperty.ImageList: TCustomImageList;
var
  comp: TPersistent;
begin
  Result := nil;
  comp := GetComponent(0);
  if Assigned(comp) then
  begin
    Result := TCustomImageList(TypInfo.GetObjectProp(comp, 'Images'));
  end;
end;

function TAdvImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect, paRevertable];
end;

function TAdvImageIndexProperty.GetValue: string;
begin
  Result := IntToStr(GetOrdValue);
end;

procedure TAdvImageIndexProperty.SetValue(const Value: string);
var
  XValue: Integer;
begin
  try
    XValue := strToInt(Value);
    SetOrdValue(XValue);
  except
    inherited SetValue(Value);
  end;
end;

procedure TAdvImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  Tmp: TCustomImageList;
  I: Integer;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    for I := 0 to Tmp.Count - 1 do
      Proc(intToStr(I));
end;

procedure TAdvImageIndexProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AWidth := Tmp.Width + ACanvas.TextHeight(Value) + 4;
end;

procedure TAdvImageIndexProperty.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AHeight := Max(Tmp.Height + 2, ACanvas.TextHeight(Value) + 2);
end;

procedure TAdvImageIndexProperty.ListDrawValue(const Value: string; ACanvas:
  TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Tmp: TCustomImageList;
  R: TRect;

begin
  Tmp := ImageList;
  if Tmp <> nil then
  begin
    R := ARect;

    ACanvas.Brush.Style := bsSolid;

    if ASelected then
    begin
      ACanvas.Brush.Color := clHighlight;
    end
    else
    begin
      ACanvas.Brush.Color := clWindow;
    end;

    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.FillRect(ARect);

    if ARect.Bottom - ARect.Top >= tmp.Height then
       Tmp.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(Value));

    OffsetRect(R, Tmp.Width + 2, 0);

    if ASelected then
      ACanvas.Font.Color := clHighlightText
    else
      ACanvas.Font.Color := clWindowText;

    ACanvas.Brush.Style := bsClear;
    DrawText(ACanvas.Handle, PChar(Value), -1, R, 0);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clWhite;
  end
  else
    DefaultPropertyListDrawValue(Value, ACanvas, ARect, ASelected);
end;

procedure TAdvImageIndexProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TAdvImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if (GetVisualValue <> '') and Assigned(Tmp) then
    ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

end.
