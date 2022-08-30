{********************************************************************}
{ TAdvFilterPanelButton component                                    }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2014 - 2015                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvFilterPanelButtonRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvFilterPanelButton, AdvCustomFilterPanel, GDIPicDE, Classes, GDIPicture,
  DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvFilterPanelButton, 'ButtonAddIcon', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvFilterPanelButton, 'ButtonRemoveIcon', TGDIPPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGridFilterDialog, 'ButtonAddIcon', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGridFilterDialog, 'ButtonRemoveIcon', TGDIPPictureProperty);
end;


end.

