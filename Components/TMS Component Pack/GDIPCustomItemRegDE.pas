{*************************************************************************}
{ TCustomItem Base Class Design time reg                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
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

unit GDIPCustomItemRegDE;

interface
{$I TMSDEFS.INC}

uses
  Classes, GDIPCustomItemDE,
  GDIPImageSectionItem, GDIPImageTextItem, GDIPGraphicItem,
  GDIPButtonBarItem,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


procedure Register;

implementation


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TImageTextItem,'ImageName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TImageSectionItem,'ImageName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'UnCheckedDisabledName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'UnCheckedNormalName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'UnCheckedHoverName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'UnCheckedDownName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'CheckedDisabledName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'CheckedNormalName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'CheckedHoverName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TGraphicGlyphs,'CheckedDownName',TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string),TButtonBarElement, 'ImageName', TPictureContainerTextProperty);
end;


end.

