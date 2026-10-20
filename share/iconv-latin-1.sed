# sed script used by iconv-latin-1.  It replaces characters that iconv's
# transliteration to Latin 1 would turn into "?".

# Box drawing
s/═/=/g
s/║/|/g
s/[╔╗╚╝╠╣╦╩╬]/+/g
s/█/X/g

# Marks and symbols
s/[✓✔✅]/yes/g
s/[✗✘❌]/no/g
s/[▶▸‣]/>/g
s/★/*/g
s/⚠/!/g
s/′/'/g
s/″/''/g

# Logic and sets
s/∈/in/g
s/∉/notin/g
s/∀/forall/g
s/∃/exists/g
s/∅/{}/g
s/⊆/subseteq/g
s/⊂/subset/g
s/⊇/supseteq/g
s/⊃/supset/g
s/∪/union/g
s/∩/intersect/g
s|∧|/\\|g
s|∨|\\/|g
s/⊢/|-/g
s/↦/|->/g
s/≡/==/g
s/≈/~=/g
s/∘/o/g
s/∞/infinity/g
s/⊏/\&sqsub;/g
s/⊑/\&sqsube;/g
s/⊐/\&sqsup;/g
s/⊒/\&sqsupe;/g
s/⊤/T/g
s/⊥/_|_/g

# Greek letters
s/Α/Alpha/g
s/α/alpha/g
s/Β/Beta/g
s/β/beta/g
s/Γ/Gamma/g
s/γ/gamma/g
s/Δ/Delta/g
s/δ/delta/g
s/Ε/Epsilon/g
s/ε/epsilon/g
s/Ζ/Zeta/g
s/ζ/zeta/g
s/Η/Eta/g
s/η/eta/g
s/Θ/Theta/g
s/[θϑ]/theta/g
s/Ι/Iota/g
s/ι/iota/g
s/Κ/Kappa/g
s/κ/kappa/g
s/Λ/Lambda/g
s/λ/lambda/g
s/Μ/Mu/g
s/μ/mu/g
s/Ν/Nu/g
s/ν/nu/g
s/Ξ/Xi/g
s/ξ/xi/g
s/Ο/Omicron/g
s/ο/omicron/g
s/Π/Pi/g
s/π/pi/g
s/Ρ/Rho/g
s/ρ/rho/g
s/Σ/Sigma/g
s/[σς]/sigma/g
s/Τ/Tau/g
s/τ/tau/g
s/Υ/Upsilon/g
s/υ/upsilon/g
s/Φ/Phi/g
s/[φϕ]/phi/g
s/Χ/Chi/g
s/χ/chi/g
s/Ψ/Psi/g
s/ψ/psi/g
s/Ω/Omega/g
s/ω/omega/g
