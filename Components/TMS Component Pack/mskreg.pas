{********************************************************************}
{ TMASKEDITEX component                                              }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit mskreg;

interface

uses
  MaskEdEx,Classes;

{$I TMSDEFS.INC}
procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TMaskEditEx]);
end;


end.
 
