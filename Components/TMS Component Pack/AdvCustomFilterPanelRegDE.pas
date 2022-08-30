{********************************************************************}
{ TAdvGDIPPicture component                                          }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2006 - 2015                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvCustomFilterPanelRegDE;

interface

uses
  AdvCustomFilterPanelDE, AdvCustomFilterPanel, Classes, GDIPicture, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomFilterPanel, 'ButtonAddIcon', TAdvCustomFilterPanelPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomFilterPanel, 'ButtonRemoveIcon', TAdvCustomFilterPanelPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGridFilterDialog, 'ButtonAddIcon', TAdvCustomFilterPanelPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvCustomGridFilterDialog, 'ButtonRemoveIcon', TAdvCustomFilterPanelPictureProperty);
end;

end.

