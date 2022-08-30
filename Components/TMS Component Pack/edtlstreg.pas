{********************************************************************}
{ TEDITLISTBOX component                                             }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit edtlstreg;

interface

{$I TMSDEFS.INC}
uses
  EditList, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TEditListBox]);
end;


end.
 
