//---------------------------------------------------------------------------


#ifndef SampleDockingTreeViewH
#define SampleDockingTreeViewH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TSampleDockingTreeViewFrame : public TForm
{
__published:	// IDE-managed Components
  TTreeView *TreeView;
private:	// User declarations
public:		// User declarations
  __fastcall TSampleDockingTreeViewFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSampleDockingTreeViewFrame *SampleDockingTreeViewFrame;
//---------------------------------------------------------------------------
#endif
