{********************************************************************}
{ TAdvPreviewMenu component                                          }
{ for Delphi & C++Builder                                            }
{ version 1.0                                                        }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2006                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvPreviewMenuRegDE;

interface
{$I TMSDEFS.INC}
uses
  AdvPreviewMenu, GDIPicDE, Classes, GDIPicture, AdvHintInfo, AdvPreviewMenuDE, 
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
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TButtonCollectionItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TButtonCollectionItem, 'DisabledPicture', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvPreviewSubMenuItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvPreviewSubMenuItem, 'DisabledPicture', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvPreviewMenuItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvPreviewMenuItem, 'DisabledPicture', TGDIPPictureProperty);

  //RegisterPropertyEditor(TypeInfo(TAdvPreviewMenuItems), TAdvPreviewMenu, 'MenuItems', TPreviewMenuProperty);
  RegisterComponentEditor(TAdvPreviewMenu, TAdvPreviewMenuEditor);
end;

end.

