{***************************************************************************}
{ TAdvHTMLPictureBrowser design time support                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvHTMLPictureBrowserDE;

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils, Types, Math, Forms, AdvHTMLPictureBrowser, HTMLDE, htmlprop, TypInfo, PictureContainer,
  DesignIntf, DesignEditors, DesignMenus, VCLEditors, Graphics, Classes, Dialogs, Controls, ImgList;

type
  TNavigationImageIndexProperty = class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
  protected
    function ImageList: TCustomImageList;
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

  TPictureContainerTextProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDescriptionHTMLTextProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TAdvHTMLPictureBrowserEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  public
  end;

implementation

{ TNavigationImageIndexProperty }

function TNavigationImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect, paRevertable];
end;

function TNavigationImageIndexProperty.GetValue: string;
begin
  Result := intToStr(GetOrdValue);
end;

procedure TNavigationImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    for I := 0 to Tmp.Count - 1 do
      Proc(IntToStr(I));
end;

function TNavigationImageIndexProperty.ImageList: TCustomImageList;
var
  comp: TPersistent;
begin
  Result := nil;
  comp := GetComponent(0);
  if comp is TNavigationBarSettings then
    Result := (comp as TNavigationBarSettings).Images;
end;

procedure TNavigationImageIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
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

procedure TNavigationImageIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AHeight := Max(Tmp.Height + 2, ACanvas.TextHeight(Value) + 2);
end;

procedure TNavigationImageIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  Tmp: TCustomImageList;
begin
  Tmp := ImageList;
  if Assigned(Tmp) then
    AWidth := Tmp.Width + ACanvas.TextHeight(Value) + 4;
end;

procedure TNavigationImageIndexProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TNavigationImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
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

procedure TNavigationImageIndexProperty.SetValue(const Value: string);
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

{ TPictureContainerTextProperty }

function TPictureContainerTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paValueEditable];
end;

procedure TPictureContainerTextProperty.GetValues(Proc: TGetStrProc);
var
  comp: TPersistent;
  i: integer;
begin
  comp := GetComponent(0);

  if Assigned(comp) and (comp is TNavigationBarSettings) then
  begin
    if Assigned((comp as TNavigationBarSettings).PictureContainer) then
    begin
      for i := 0 to (comp as TNavigationBarSettings).PictureContainer.Items.Count - 1 do
      begin
        Proc((comp as TNavigationBarSettings).PictureContainer.Items.Items[i].Name);
      end;
    end;
  end;
end;

{ TDescriptionHTMLTextProperty }

procedure TDescriptionHTMLTextProperty.Edit;
var
  HTMLEditor: THTMLEditor;
  PropInfo: PPropInfo;
  comp: TAdvHTMLPictureBrowser;
  item: THTMLPictureItem;
begin
  HTMLEditor := THTMLEditor.Create(Application);

  try
    comp := nil;

    if GetComponent(0) is THTMLPictureItem then
    begin
      item := GetComponent(0) as THTMLPictureItem;
      comp := (item.Collection as THTMLPictureList).Owner as TAdvHTMLPictureBrowser;
    end;

    if Assigned(comp) then
    begin
      HTMLEditor.Memo1.Lines.Assign(TStrings(GetOrdValue));
      {try to inherit the default font}
      PropInfo:= typInfo.GetPropInfo(comp.ClassInfo,'Font');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.Font.Assign(TFont(GetOrdProp(comp,PropInfo)));

      PropInfo:= typInfo.GetPropInfo(comp.ClassInfo,'Images');
      if (PropInfo<>nil) then
      begin
        HTMLEditor.HTMLStaticText1.Images:=(TImageList(GetOrdProp(comp,PropInfo)));
      end;

      PropInfo:= typInfo.GetPropInfo(comp.ClassInfo,'URLColor');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.URLColor:=(TColor(GetOrdProp(comp,PropInfo)));

      PropInfo:= typInfo.GetPropInfo(comp.ClassInfo,'HoverColor');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.HoverColor:=(TColor(GetOrdProp(comp,PropInfo)));

      PropInfo:= typInfo.GetPropInfo(comp.ClassInfo,'HoverFontColor');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.HoverFontColor:=(TColor(GetOrdProp(comp,PropInfo)));

      PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'Hover');
      if (PropInfo<>nil) then
       HTMLEditor.HTMLStaticText1.Hover:=(Boolean(GetOrdProp(comp,PropInfo)));

      PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'AnchorHint');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.AnchorHint := (Boolean(GetOrdProp(comp,PropInfo)));

      PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'ShadowColor');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.ShadowColor := (TColor(GetOrdProp(comp,PropInfo)));

      PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'ShadowOffset');
      if (PropInfo<>nil) then
        HTMLEditor.HTMLStaticText1.ShadowOffset := (integer(GetOrdProp(comp,PropInfo)));

      PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'PictureContainer');
      if PropInfo <> nil then
        HTMLEditor.HTMLStaticText1.PictureContainer := (TPictureContainer(GetOrdProp(comp,PropInfo)));
    end;

    if HTMLEditor.Showmodal = mrOK then
      SetOrdValue(longint(HTMLEditor.Memo1.Lines));
  finally
    HTMLEditor.Free;
  end;
end;

function TDescriptionHTMLTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

function TDescriptionHTMLTextProperty.GetValue: String;
begin
  Result := '(HTMLText)';
end;

procedure TDescriptionHTMLTextProperty.SetValue(const Value: String);
begin
end;

{ TAdvHTMLPictureBrowserEditor }

procedure TAdvHTMLPictureBrowserEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'Pictures') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

end.
