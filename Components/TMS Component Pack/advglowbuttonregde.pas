{********************************************************************}
{ TAdvGlowButton component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2006 - 2013                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvGlowButtonRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvGlowButton, GDIPicDE, Classes, GDIPicture, AdvHintInfo, ImgList,
  DesignIntf, DesignEditors
{$IFDEF DELPHIXE3_LVL}
  , System.UITypes
{$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGlowButton, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGlowButton, 'DisabledPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvHintInfo, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGlowButton, 'HotPicture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TAdvCustomGlowButton, 'ImageIndex', TAdvImageIndexProperty);
end;


end.

