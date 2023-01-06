#!/bin/env python
import os
import sys
import drawSvg as draw


annotation_colors = {
    'DNA metabolism': '#80c476',#38c172',
    'Structure': '#f76459',#f66d9b',
    'Other': '#757575',
}

annotation_border_colors = {
    'DNA metabolism': '#53A247',
    'Structure': '#E04033',
    'Other': '#757575',
}

def identity_to_color(ident):
    col_dec = 255 - int((ident / 100) * 200)
    col_hex = str(hex(col_dec))[2:]
    return f"#{col_hex}{col_hex}{col_hex}"

class MultiSyntenyPlot:

    def __init__(self, width, height, max_genome_size, x_padding = 60, y_padding = 20, space_between_genomes = 100, genome_height = 15):
        self.width = width + 300
        self.height = height
        self.x_padding = x_padding
        self.y_padding = y_padding
        self.x_scale = (width - x_padding * 2) / max_genome_size
        self.space_between_genomes = space_between_genomes
        self.genome_height = genome_height
        self.y_slot = 0
        self.drawing = draw.Drawing(self.width, self.height, displayInline=False)
        self.drawing.draw(draw.Rectangle(0,0,self.width,self.height, fill='#fff'))


    def add_pairwise(self, blocks):
        y_origin = self.height - self.y_padding - self.genome_height - self.y_slot * (self.genome_height + self.space_between_genomes)
        for b in blocks:
            color = identity_to_color(b["ident"])

            self.drawing.append(draw.Lines(self.x_padding + b["q_start"] * self.x_scale, y_origin,
                                           self.x_padding + b["q_end"]   * self.x_scale, y_origin,
                                           self.x_padding + b["s_end"]   * self.x_scale, y_origin - self.space_between_genomes,
                                           self.x_padding + b["s_start"] * self.x_scale, y_origin - self.space_between_genomes,
                                           close = True,
                                           fill = color,
                                           stroke = 'none'))

        self.y_slot += 1


    def draw_functional_labels(self):
        y_origin = self.height - self.y_padding - 10
        x_origin = self.width - 300 - self.x_padding + 7

        row_idx = 0
        self.drawing.append(draw.Text("Functional annotation", 20, x_origin, y_origin - row_idx * 20, fill='black'))
        row_idx += 1.5
        for label, color in annotation_colors.items():
            self.drawing.append(draw.Rectangle(x_origin, y_origin - row_idx * 23, 17, 17, fill=color))
            self.drawing.append(draw.Text(label, 16, x_origin + 23, y_origin - row_idx * 23 + 3, fill='black'))
            row_idx += 1

        row_idx += 2
        self.drawing.append(draw.Text("Synteny, protein identity (%)", 20, x_origin, y_origin - row_idx * 20, fill='black'))
        row_idx += 2
        for i, ident in enumerate([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]):
            col = identity_to_color(ident)
            self.drawing.append(draw.Rectangle(x_origin + i*25, y_origin - row_idx * 20, 25, 25, fill=col, stroke=col))
            if i % 2 == 0:
                self.drawing.append(draw.Text(str(ident), 16, x_origin + (i+0.5) * 25, y_origin - row_idx * 20 - 15, fill='black', text_anchor='middle'))
        self.drawing.append(draw.Rectangle(x_origin, y_origin - row_idx * 20, 11*25, 25, stroke="black", fill="none"))


    def add_genes(self, sample_id, gff_filename, color, border_color, funtional_annotation_filename):

        y_origin = self.height - self.y_padding - self.genome_height - self.y_slot * (self.genome_height + self.space_between_genomes) - 0.5
        self.drawing.append(draw.Text(sample_id, 20, self.x_padding - 6, y_origin, fill='black', text_anchor='end'))

        functional_annotation = {}
        if funtional_annotation_filename:
            with open(funtional_annotation_filename, "r") as func_file:
                for line in func_file:
                    data = line.strip().split("\t")
                    region = data[0]+"-"+data[1]
                    if len(data) > 4:
                        functional_annotation[region] = data[4]


        with open(gff_filename, "r") as gff_file:
            for line in gff_file:
                if line.startswith("#"):
                    continue

                data = line.split("\t")
                feature_start = int(data[3])
                feature_end = int(data[4])
                region_str = data[3]+"-"+data[4]
                strand = data[6]

                if not functional_annotation:
                    feature_color = color
                    feature_border_color = border_color
                else:
                    if region_str in functional_annotation and region_str != "Other":
                        feature_color = annotation_colors[functional_annotation[region_str]]
                        feature_border_color = annotation_border_colors[functional_annotation[region_str]]
                    else:
                        feature_color = annotation_colors["Other"]
                        feature_border_color = annotation_border_colors["Other"]

                if strand == "+":
                    self.drawing.append(draw.Lines(self.x_padding + feature_start * self.x_scale, y_origin + 1,
                                                   self.x_padding + feature_end * self.x_scale - 3, y_origin + 1,
                                                   self.x_padding + feature_end * self.x_scale - 1, y_origin + self.genome_height/2,
                                                   self.x_padding + feature_end * self.x_scale - 3, y_origin + self.genome_height,
                                                   self.x_padding + feature_start * self.x_scale, y_origin + self.genome_height,
                                                   close = True,
                                                   fill = feature_color,
                                                   stroke = feature_border_color))

                if strand == "-":
                    self.drawing.append(draw.Lines(self.x_padding + feature_end * self.x_scale, y_origin + 1,
                                                   self.x_padding + feature_start * self.x_scale + 3, y_origin + 1,
                                                   self.x_padding + feature_start * self.x_scale + 1, y_origin + self.genome_height/2,
                                                   self.x_padding + feature_start * self.x_scale + 3, y_origin + self.genome_height,
                                                   self.x_padding + feature_end * self.x_scale, y_origin + self.genome_height,
                                                   close = True,
                                                   fill = feature_color,
                                                   stroke = feature_border_color))


                gene_id = data[8].split(";")[0].split(".")[-1]



    def finalize(self, filename):
        self.drawing.setPixelScale(2)
        self.drawing.savePng(filename)
#        self.drawing.saveSvg(filename)


def main():

    plot = MultiSyntenyPlot(2000, 650, 170000)

    order = ["T2_rot", "a15", "a51", "a48", "a20", "RB69_rot"]
    colors = {
        "T2_rot":"#aaaaaa",
        "a15":"#9ED8D8",
        "a20":"#F7B5C4",
        "a48":"#ADE88A",
        "a51":"#FFC698",
        "RB69_rot":"#aaaaaa",
    }

    border_colors = {
        "T2_rot":"#888888",
        "a15":"#429191",
        "a20":"#DE6581",
        "a48":"#6AB53E",
        "a51":"#D68C4E",
        "RB69_rot":"#888888",
    }

    functional_annotations = {
        "T2_rot": "T2_annotation_groups_emre.tsv",
        "RB69_rot": "RB69_annotation_groups_emre.tsv",
    }

    display_name = {
        "T2_rot":"T2",
        "a15":"α15",
        "a20":"α20",
        "a48":"α48",
        "a51":"α51",
        "RB69_rot":"RB69",
    }

    i = 0
    for i in range(0,len(order)):
        sample = order[i]
        gff_filename = f"{sample}.gff"
        gene_positions = parse_gff(gff_filename)
        plot.add_genes(display_name[sample], gff_filename, colors[sample], border_colors[sample], functional_annotations.get(sample))

        if i >= len(order)-1:
            continue

        blast_filename = f"{sample}.faa.{order[i+1]}.fa.tblastn"
        blocks = []
        prev_gene_name = ""
        with open(blast_filename, "r") as blast_file:
            for line in blast_file:
                line = line.strip()
                data = line.split("\t")
                gene_name = data[0]
                if gene_name == prev_gene_name:
                    continue

                gene_info = next((gene for gene in gene_positions if gene["name"] == gene_name), None)
                if gene_info is None:
                    print(f"WARNING: NO GFF ENTRY FOR {gene_name}")
                    continue


                block = {}

                query_hit_genomic_start = gene_info["start"] + (int(data[6]) - 1) * 3
                query_hit_genomic_end = gene_info["start"] + (int(data[7]) - 1) * 3
                if gene_info["strand"] == "+":
                    block["q_start"] = query_hit_genomic_start
                    block["q_end"] = query_hit_genomic_end
                else:
                    block["q_start"] = query_hit_genomic_end
                    block["q_end"] = query_hit_genomic_start

                block["s_start"] = int(data[8])
                block["s_end"] = int(data[9])
                block["ident"] = float(data[2])
                q_size = abs(block["q_start"] - block["q_end"])

                if not overlaps_any(block, blocks):
                    blocks.append(block)

                prev_gene_name = gene_name

        plot.add_pairwise(blocks)

    plot.draw_functional_labels()
    plot.finalize("test.png")


def parse_gff(filename):
    genes = []
    with open(filename, "r") as gff_file:
        for line in gff_file:
            if line.startswith("#"):
                continue

            data = line.split("\t")
            name = data[8].split("\t")[0].split(";")[0].split("=")[1]

            genes.append({
                'name': name,
                'start': int(data[3]),
                'end': int(data[4]),
                'strand': data[6]
            })

    return genes

def overlaps_any(new_block, blocks):
    new_q_max = max(new_block["q_start"], new_block["q_end"])
    new_q_min = min(new_block["q_start"], new_block["q_end"])
    new_size = new_q_max - new_q_min

    for old_block in blocks:
        old_q_max = max(old_block["q_start"], old_block["q_end"])
        old_q_min = min(old_block["q_start"], old_block["q_end"])

        if (new_q_max > old_q_min and new_q_min < old_q_max):
            overlap_bases = min(old_q_max,new_q_max) - max(old_q_min,new_q_min)
            if overlap_bases / new_size > 0.1:
                return True

    return False


if __name__ == "__main__":
    main()
