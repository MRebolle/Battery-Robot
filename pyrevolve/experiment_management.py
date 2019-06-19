import os
import shutil

class ExperimentManagement:

    def __init__(self, settings):
        self.settings = settings

    def create_exp_folders(self):
        dirpath = 'experiments/'+self.settings.experiment_name
        if os.path.exists(dirpath):
            shutil.rmtree(dirpath)
        os.mkdir(dirpath)
        os.mkdir(dirpath+'/data_fullevolution')
        os.mkdir(dirpath+'/data_fullevolution/genotypes')
        os.mkdir(dirpath+'/data_fullevolution/phenotypes')
        os.mkdir(dirpath+'/data_fullevolution/descriptors')
        os.mkdir(dirpath+'/data_fullevolution/fitness')
        os.mkdir(dirpath+'/data_fullevolution/phenotype_images')
        os.mkdir(dirpath+'/data_fullevolution/failed_eval_robots')

    def export_genotype(self, individual):
        if self.settings.recovery_enabled:
            individual.genotype.export_genotype('experiments/'+self.settings.experiment_name
                                                +'/data_fullevolution/genotypes/genotype_'+str(individual.genotype.id)+'.txt')

    def export_phenotype(self, individual):
        if self.settings.export_phenotype:
            individual.phenotype.save_file('experiments/'+self.settings.experiment_name
                                           +'/data_fullevolution/phenotypes/phenotype_'+str(individual.genotype.id)+'.yaml')

    def export_fitnesses(self, individuals):
        for individual in individuals:
            f = open(f'experiments/{self.settings.experiment_name}/data_fullevolution/fitness/fitness_{individual.genotype.id}.txt', "w")
            f.write(str(individual.fitness))
            f.close()

    def export_phenotype_images(self, dirpath, individual):
        individual.phenotype.render_body('experiments/'+self.settings.experiment_name +'/'+dirpath+'/body_'+str(individual.phenotype.id)+'.png')
        individual.phenotype.render_brain('experiments/'+self.settings.experiment_name +'/'+dirpath+'/brain_' + str(individual.phenotype.id))

    def export_failed_eval_robot(self,individual):
        individual.genotype.export_genotype(f'experiments/{self.settings.experiment_name}/data_fullevolution/failed_eval_robots/genotype_{str(individual.genotype.id)}.txt')
        individual.phenotype.save_file(f'experiments/{self.settings.experiment_name}/data_fullevolution/failed_eval_robots/phenotype_{str(individual.genotype.id)}.yaml')
        individual.phenotype.save_file(f'experiments/{self.settings.experiment_name}/data_fullevolution/failed_eval_robots/phenotype_{str(individual.genotype.id)}.sdf', conf_type='sdf')


    def export_snapshots(self, individuals, gen_num):
        if self.settings.recovery_enabled:
            dirpath = self.settings.experiment_name+'/selectedpop_'+str(gen_num)
            if os.path.exists('experiments/'+dirpath):
                shutil.rmtree('experiments/'+dirpath)
            os.mkdir('experiments/'+dirpath)
            for ind in individuals:
                self.export_phenotype_images('selectedpop_'+str(gen_num), ind)

    def experiment_is_new(self):
        if os.path.isfile('experiments/{}/selectedpop_to_recover.txt'.format(self.settings.experiment_name)):
            return False
        else:
            return True

    def update_recovery_state(self, generation, next_id):
        # contains the number of the lastest surviving population, and the id of the due next genotype
        with open('experiments/{}/selectedpop_to_recover.txt'.format(self.settings.experiment_name), "w") as f:
            f.write("{} {}".format(generation, next_id))

    def read_recovery_state(self):
        with open('experiments/{}/selectedpop_to_recover.txt'.format(self.settings.experiment_name), "r") as f:
            contents = f.read()
        state = contents.split(' ')
        return int(state[0]), int(state[1])



