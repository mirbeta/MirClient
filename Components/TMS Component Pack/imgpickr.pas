{********************************************************************}
{ TImagePicker component                                             }
{ for Delphi  & C++Builder                                           }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2001                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit imgpickr;

{$I TMSDEFS.INC}
interface

uses
  ImagePicker,Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TImagePicker]);
end;



end.

