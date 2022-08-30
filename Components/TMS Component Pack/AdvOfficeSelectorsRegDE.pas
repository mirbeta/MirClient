{********************************************************************}
{ TAdvOfficeSelector components                                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2006 - 2014                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvOfficeSelectorsRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvOfficeSelectors, Classes, DesignIntf, DesignEditors;

type

  TAdvOfficeScrollSelectorEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


procedure Register;

implementation


uses
  SysUtils, GDIPicture, GDIPicDE, AdvStyleIF;


{ TAdvOfficeScrollSelectorEditor }

procedure TAdvOfficeScrollSelectorEditor.ExecuteVerb(Index: Integer);
begin
  inherited;

  if (Component is TAdvCustomOfficeScrollSelector) then
  begin
      if (Index in [0..7]) or (Index in [9..19]) then
        (Component as TAdvCustomOfficeScrollSelector).SetComponentStyle(TTMSStyle(Index));
      (Component as TAdvCustomOfficeScrollSelector).Repaint;
      Exit;
    end;
end;

function TAdvOfficeScrollSelectorEditor.GetVerb(Index: Integer): string;
begin
  if (Component is TAdvCustomOfficeScrollSelector) then
  begin
    //tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsOffice2003Classic,
    //tsOffice2007Luna, tsOffice2007Obsidian, tsWindowsXP, tsWhidbey
    case Index of
    0: Result := 'Office 2003 Blue';
    1: Result := 'Office 2003 Silver';
    2: Result := 'Office 2003 Olive';
    3: Result := 'Office 2003 Classic';
    4: Result := 'Office 2007 Luna';
    5: Result := 'Office 2007 Obsidian';
    6: Result := 'Windows XP';
    7: Result := 'Whidbey';
    8: Result := 'Custom';
    9: Result := 'Office 2007 Silver';
    10: Result := 'Windows Vista';
    11: Result := 'Windows 7';
    12: Result := 'Terminal';
    13: Result := 'Office 2010 Blue';
    14: Result := 'Office 2010 Silver';
    15: Result := 'Office 2010 Black';
    16: Result := 'Windows 8';
    17: Result := 'Office 2013 White';
    18: Result := 'Office 2013 Light gray';
    19: Result := 'Office 2013 Gray';
    end;
  end;
end;

function TAdvOfficeScrollSelectorEditor.GetVerbCount: Integer;
begin
  Result := 8;
end;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenStyleSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenStyleSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeBrushStyleSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeBrushStyleSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeShadowSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeShadowSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableBorderSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableBorderSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeGradientDirectionSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeGradientDirectionSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenWidthSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficePenWidthSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeColorSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeColorSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTextColorSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTextColorSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeToolSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeToolSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeTableSelector, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeCharacterSelector, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvOfficeCharacterSelector, 'DisabledPicture', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvScrollSelectorItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvSelectorItem, 'Picture', TGDIPPictureProperty);

  RegisterComponentEditor(TAdvCustomOfficeScrollSelector,TAdvOfficeScrollSelectorEditor);
end;

end.

