{********************************************************************}
{ TEDITBUTTON component                                              }
{ for Delphi & C++ Builder                                           }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}
unit edtbreg;

{$I TMSDEFS.INC}
interface

uses
  EditBtn, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TEditBtn,TUnitEditBtn]);
end;


end.
 
