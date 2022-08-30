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
unit AdvHTMLPictureBrowserRegDE;

{$I TMSDEFS.INC}

interface

uses
  Classes, GDIPicDE, AdvHTMLPictureBrowser, HTMLDE, AdvHTMLPictureBrowserDE, DesignIntf, DesignEditors,
  GDIPicture, ImgList
  {$IFDEF DELPHIXE3_LVL}
  , UITypes
  {$ENDIF}
 ;


procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStringList), THTMLPictureItem, 'HTMLDescription', TDescriptionHTMLTextProperty);
  RegisterComponentEditor(TAdvHTMLPictureBrowser, TAdvHTMLPictureBrowserEditor);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvHTMLPictureBrowser, '', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), THTMLPictureItem, '', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TNavigationBarSettings, '', TNavigationImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TNavigationBarSettings, 'NameImageBullet', TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TNavigationBarSettings, 'NameImageFirst', TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TNavigationBarSettings, 'NameImageLast', TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TNavigationBarSettings, 'NameImageNext', TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TNavigationBarSettings, 'NameImagePrevious', TPictureContainerTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TNavigationBarSettings, 'NameImageSelectedBullet', TPictureContainerTextProperty);
end;

end.
