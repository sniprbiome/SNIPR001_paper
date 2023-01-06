makeblastdb -in RB69_rot.fa -dbtype nucl -parse_seqids
makeblastdb -in a15.fa -dbtype nucl -parse_seqids
makeblastdb -in a20.fa -dbtype nucl -parse_seqids
makeblastdb -in T2_rot.fa -dbtype nucl -parse_seqids
makeblastdb -in a48.fa -dbtype nucl -parse_seqids
makeblastdb -in a51.fa -dbtype nucl -parse_seqids

for a in RB69_rot.faa a15.faa a20.faa T2_rot.faa a48.faa a51.faa; do
    for b in RB69_rot.fa a15.fa a20.fa T2_rot.fa a48.fa a51.fa; do
        tblastn -query $a -db $b -out $a.$b.tblastn -outfmt 6 -evalue 1e-10
    done
done

