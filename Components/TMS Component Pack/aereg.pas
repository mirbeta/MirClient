{***********************************************************************}
{ TADVEDIT component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 1996-2015                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}
unit aereg;

interface
{$I TMSDEFS.INC}

uses
  AdvEdit, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvEdit,TAdvMaskEdit]);
end;



end.

