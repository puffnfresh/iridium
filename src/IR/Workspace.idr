module IR.Workspace

import IR

manage : Eq wid => wid -> Workspace wid -> Workspace wid
manage wid (MkWorkspace l Nothing) = MkWorkspace l (Just (MkStack wid [] []))
manage wid (MkWorkspace l (Just (MkStack wid' a b))) = MkWorkspace l (Just (MkStack wid a (wid' :: b)))
