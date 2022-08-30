{**************************************************************************}
{ TAdvSmoothTileListDE DESIGN TIME EDITOR                                  }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2011                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothTileListDE;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Graphics, ImgList, Math, AdvSmoothTileList,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  Classes, Dialogs, Controls;

type
  TAdvSmoothTileListEditor = class(TDefaultEditor)
  protected
    {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
    {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TAdvSmoothTileListImageIndexProperty = class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
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
  TypInfo, SysUtils, AdvSmoothStyles, AdvStyleIF;

{$IFDEF DELPHI6_LVL}
procedure TAdvSmoothTileListEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TAdvSmoothTileListEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'TILES') = 0) then
    begin
      PropertyEditor.Edit;
      Continue := False;
    end;
end;

procedure TAdvSmoothTileListEditor.ExecuteVerb(Index: integer);
var
  psf: TAdvSmoothStyleForm;
  style: TTMSStyle;
begin
  inherited;
  if (Index = 0) then
  begin
    style := (Component as TAdvSmoothTileList).GetComponentStyle;

    psf := TAdvSmoothStyleForm.Create(Application);
    case style of
      tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
      tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
      tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
      tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
      tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
      tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
      tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
      tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
      tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
      tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
      tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
      tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
      tsWindows7: psf.RadioGroup1.ItemIndex := 12;
      tsTerminal: psf.RadioGroup1.ItemIndex := 13;
      tsWindows8: psf.RadioGroup1.ItemIndex := 14;
      tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
      tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
      tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
      tsWindows10: psf.RadioGroup1.ItemIndex := 18;
      tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
      tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
      tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
    end;

    if psf.ShowModal = mrOK then
    begin
      case psf.RadioGroup1.ItemIndex of
        0: style := tsOffice2003Blue;
        1: style := tsOffice2003Olive;
        2: style := tsOffice2003Silver;
        3: style := tsOffice2003Classic;
        4: style := tsOffice2007Luna;
        5: style := tsOffice2007Obsidian;
        6: style := tsOffice2007Silver;
        7: style := tsOffice2010Blue;
        8: style := tsOffice2010Silver;
        9: style := tsOffice2010Black;
        10: style := tsWindowsXP;
        11: style := tsWindowsVista;
        12: style := tsWindows7;
        13: style := tsTerminal;
        14: style := tsWindows8;
        15: style := tsOffice2013White;
        16: style := tsOffice2013LightGray;
        17: style := tsOffice2013Gray;
        18: style := tsWindows10;
        19: style := tsOffice2016White;
        20: style := tsOffice2016Gray;
        21: style := tsOffice2016Black;
      end;
      if (Component is TAdvSmoothTileList) then
         (Component as TAdvSmoothTileList).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
  end;
end;

function TAdvSmoothTileListEditor.GetVerb(index: integer): string;
begin
  if index = 0 then
    Result := 'Styles';
end;

function TAdvSmoothTileListEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TAdvSmoothTileListImageIndexProperty.ImageList: TCustomImageList;
var
  cnt: TAdvSmoothTileContent;
  comp: TPersistent;
begin
  Result := nil;
  cnt := nil;
  comp := GetComponent(0);
  if comp is TAdvSmoothTileContent then
    cnt := comp as TAdvSmoothTileContent;

  if Assigned(cnt) then
    Result := TCustomImageList(TypInfo.GetObjectProp(cnt.TileList, 'ImageList'));
end;

function TAdvSmoothTileListImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect, paRevertable];
end;

function TAdvSmoothTileListImageIndexProperty.GetValue: string;
begin
  Result := intToStr(GetOrdValue);
end;

procedure TAdvSmoothTileListImageIndexProperty.SetValue(const Value: string);
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

procedure TAdvSmoothTileListImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  Tmp: TCustomImageList;
  I: Integer;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    for I := 0 to Tmp.Count - 1 do
      Proc(intToStr(I));
end;

procedure TAdvSmoothTileListImageIndexProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AWidth := Tmp.Width + ACanvas.TextHeight(Value) + 4;
end;

procedure TAdvSmoothTileListImageIndexProperty.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AHeight := Max(Tmp.Height + 2, ACanvas.TextHeight(Value) + 2);
end;

procedure TAdvSmoothTileListImageIndexProperty.ListDrawValue(const Value: string; ACanvas:
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

procedure TAdvSmoothTileListImageIndexProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TAdvSmoothTileListImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
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








