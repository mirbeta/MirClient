{****************************************************************}
{ TWebImage component                                            }
{ for Delphi & C++Builder                                        }
{ version 1.1                                                    }
{                                                                }
{ written by                                                     }
{   TMS Software                                                 }
{   copyright © 2000-2004                                        }
{   Email : info@tmssoftware.com                                 }
{   Web : http://www.tmssoftware.com                             }
{****************************************************************}

unit WebImgRegDE;

interface
{$I TMSDEFS.INC}
uses
  WebImage, WebImgDE, Classes,
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
  RegisterPropertyEditor(TypeInfo(TWebPicture), TWebImage, 'WebPicture', TWebPictureProperty);
end;

end.

